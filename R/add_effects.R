#' add_autoregressive_cross_lagged
#'
#' Creates autoregressive and cross-lagged effects for the observed variables
#' @param internal_list internal list with info_variables
#' @returns data.frame with autoregressive and cross-lagged effects
#' @keywords internal
add_autoregressive_cross_lagged <- function(internal_list){

  observed      <- internal_list$info_variables$user_names_time_varying
  process_names <- internal_list$info_variables$names_processes["user_names",]
  effects       <- data.frame()

  for(i in 1:(ncol(observed)-1)){
    outgoing <- observed[,i]
    incoming <- observed[,i+1]

    for(out in outgoing){
      for(inc in incoming){

        effects <- rbind(effects,
                         data.frame(
                           outgoing = out,
                           incoming = inc,
                           type     = "directed",
                           op       = "~",
                           location = "C",
                           label    = paste0("c_",
                                             process_names[[which(incoming == inc)]],
                                             "_",
                                             process_names[[which(outgoing == out)]]),
                           value   = .1,
                           algebra = "",
                           free    = TRUE
                         ))

      }
    }

  }

  rownames(effects) <- NULL
  return(effects)

}

#' add_process_residual_variances
#'
#' Creates residual variances for the observed variables
#' @param internal_list internal list with info_variables
#' @returns data.frame with residual variances
#' @keywords internal
add_process_residual_variances <- function(internal_list){

  observed      <- internal_list$info_variables$user_names_time_varying
  process_names <- internal_list$info_variables$names_processes["user_names",]
  effects       <- data.frame()

  for(i in 1:ncol(observed)){
    outgoing <- observed[,i]
    incoming <- outgoing

    if(i == 1){
      # treat first occasion differently
      for(out_index in 1:length(outgoing)){
        for(inc_index in out_index:length(outgoing)){
          # we add all variances and covariances
          effects <- rbind(effects,
                           expand.grid(
                             outgoing = outgoing[[out_index]],
                             incoming = incoming[[inc_index]],
                             type     = "undirected",
                             op       = "~~",
                             location = "Psi",
                             label    = paste0("psi_",
                                               incoming[[inc_index]],
                                               "_",
                                               outgoing[[out_index]]),
                             value   = .6,
                             algebra = "",
                             free = TRUE
                           )
          )
        }
      }
      # go to next iteration
      next
    }

    effects <- rbind(effects,
                     data.frame(
                       outgoing = outgoing,
                       incoming = incoming,
                       type     = "undirected",
                       op       = "~~",
                       location = "Psi",
                       label    = paste0("psi_",
                                         process_names,
                                         "_",
                                         process_names),
                       value   = .6,
                       algebra = "",
                       free = TRUE
                     )
    )

  }

  rownames(effects) <- NULL
  return(effects)

}

#' add_latent_trait
#'
#' Creates latent traits for each of the observed variables
#' @param internal_list internal list with info_variables
#' @returns data.frame with latent traits
#' @keywords internal
add_latent_trait <- function(internal_list){

  observed      <- internal_list$info_variables$user_names_time_varying
  process_names <- internal_list$info_variables$names_processes["user_names",]
  effects       <- data.frame()

  additive      <- internal_list$info_model$heterogeneity == "additive"
  if(!additive){
    warning("Adding non-additive latent processes.")
  }

  etas             <- paste0("eta", process_names)
  etas_nonadditive <- c()

  if(!additive){
    for(i in 1:length(process_names)){
      for(j in 1:length(process_names))
        if(i != j)
          etas_nonadditive <- c(etas_nonadditive,
                                paste0("eta", process_names[[i]], process_names[[j]])
          )
    }
  }

  for(i in 1:ncol(observed)){
    incoming <- observed[,i]

    if(i == 1){
      # treat first occasion differently
      for(out in c(etas, etas_nonadditive)){
        for(inc in incoming){

          effects <- rbind(effects,
                           data.frame(
                             outgoing = out,
                             incoming = inc,
                             type     = "directed",
                             op       = "=~",
                             location = "C",
                             label    = paste0("c_",
                                               inc,
                                               "_",
                                               out),
                             value   = .5,
                             algebra = "",
                             free = TRUE
                           ))

        }
      }

      # go to next iteration
      next
    }

    for(j in 1:length(incoming)){
      effects <- rbind(effects,
                       data.frame(
                         outgoing = etas[[j]],
                         incoming = incoming[[j]],
                         type     = "directed",
                         op       = "=~",
                         location = "C",
                         label    = "",
                         value    = 1,
                         algebra  = "",
                         free     = FALSE
                       )
      )

      if(!additive){
        previous_incoming <- observed[,i-1]
        effects <- rbind(effects,
                         data.frame(
                           outgoing = paste0(etas[[j]], process_names[which(incoming != incoming[[j]])]),
                           incoming = incoming[[j]],
                           type     = "directed",
                           op       = "=~",
                           location = "C",
                           label    = paste0("data.",previous_incoming[which(incoming != incoming[[j]])]),
                           value    = 0,
                           algebra  = "",
                           free = FALSE
                         )
        )
      }
    }
  }

  # add latent trait variances and covariances
  etas_combined <- c(etas, etas_nonadditive)
  for(out_index in 1:length(etas_combined)){
    for(inc_index in out_index:length(etas_combined)){
      # we add all variances and covariances
      effects <- rbind(effects,
                       expand.grid(
                         outgoing = etas_combined[[out_index]],
                         incoming = etas_combined[[inc_index]],
                         type     = "undirected",
                         op       = "~~",
                         location = "Psi",
                         label    = paste0("psi_",
                                           etas_combined[[inc_index]],
                                           "_",
                                           etas_combined[[out_index]]),
                         value    = .6,
                         algebra  = "",
                         free     = TRUE
                       )
      )
    }
  }

  rownames(effects) <- NULL
  return(effects)
}

#' add_exogenous_predictors
#'
#' Creates exogenous predictors as specified in internal_list$info_variables$info_time_invariant_variables
#' @param internal_list internal list with info_variables
#' @returns data.frame with exogenous predictors
#' @keywords internal
add_exogenous_predictors <- function(internal_list){

  observed      <- internal_list$info_variables$user_names_time_varying
  process_names <- internal_list$info_variables$names_processes["user_names",]
  effects       <- data.frame()

  linear <- internal_list$info_model$linear

  exogenous_predictors <- internal_list$info_variables$info_time_invariant_variables

  for(i in 1:ncol(observed)){
    incoming <- observed[,i]

    if(i == 1){
      # treat first occasion differently
      for(out in unique(unlist(exogenous_predictors))){
        for(inc in incoming){

          #if(!linear)
          #  algebra <- paste0(c_x_z1_t2 := c_x_z1 + c_x_y_z1 * data.y1)

          effects <- rbind(effects,
                           data.frame(
                             outgoing = out,
                             incoming = inc,
                             type     = "directed",
                             op       = "~",
                             location = "C",
                             label    = paste0("c_",
                                               inc,
                                               "_",
                                               out),
                             value    = .2,
                             algebra  = "",
                             free     = TRUE
                           ))

        }
      }
      # go to next iteration
      next
    }

    for(j in 1:length(incoming)){

      if(linear){
        incoming_label <- process_names[j]
        algebra        <- rep("", length(exogenous_predictors[[j]]))
      }else{
        incoming_label <- incoming[[j]]
        incoming_previous <- observed[,i-1]
        algebra <- c()
        for(ex in exogenous_predictors[[j]]){
          algebra <- c(algebra,
                       paste0("c_", process_names[j], "_", ex, " + ",
                              paste0(paste0("c_", process_names[j], "_", process_names[-j], "_", ex, " * data.", incoming_previous[-j]), collapse = " + ")
                              )
                       )
        }
      }

      effects <- rbind(effects,
                       data.frame(
                         outgoing = exogenous_predictors[[j]],
                         incoming = incoming[[j]],
                         type     = "directed",
                         op       = "~",
                         location = "C",
                         label    = paste0("c_", incoming_label, "_", exogenous_predictors[[j]]),
                         value    = 0,
                         algebra  = algebra,
                         free     = algebra == ""
                       )
      )
    }
  }

  # add exogenous variances and covariances
  unique_exogenous <- exogenous_predictors |>
    unlist() |>
    unique()
  for(out_index in 1:length(unique_exogenous)){
    for(inc_index in out_index:length(unique_exogenous)){
      # we add all variances and covariances
      effects <- rbind(effects,
                       expand.grid(
                         outgoing = unique_exogenous[[out_index]],
                         incoming = unique_exogenous[[inc_index]],
                         type     = "undirected",
                         op       = "~~",
                         location = "Psi",
                         label    = paste0("psi_",
                                           unique_exogenous[[inc_index]],
                                           "_",
                                           unique_exogenous[[out_index]]),
                         value    = ifelse(unique_exogenous[[out_index]] == unique_exogenous[[inc_index]],.6,0),
                         algebra  = "",
                         free     = TRUE
                       )
      )
    }
  }

  rownames(effects) <- NULL
  return(effects)
}
