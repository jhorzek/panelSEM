## Changelog:
# CG 0.0.1 2023-04-25: initial programming

## Documentation
#' @title Create Parameter Table
#' @description Create a table of model parameters.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @param parameter_type Character string describing the type of model matrix. Admissible values are: \code{"C"}, \code{"Psi"}.
#' @return A table of model parameters.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598


## Function definition
create_parameter_table <- function(internal_list = NULL,
                                   parameter_type = NULL){


  # function name
  fun.name <- "create_parameter_table"

  # function version
  fun.version <- "0.0.1 2023-04-25"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  # check function arguments
  ## get class of model object
  model_class <- class(internal_list)

  ## set supported classes of model objects
  supported_model_classes <- c( "panelSEM" )

  ## check if argument model is supported
  if(!any(model_class %in% supported_model_classes)) stop(
    paste0(
      fun.name.version, ": model of class ", model_class,
      " not supported. Supported fit objects are: ",
      paste(supported_model_classes, collapse = ", ")
    )
  )

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version,
                                  " ", Sys.time(), "\n" ) )

  #TODO: check data argument

  homogeneous <- internal_list$info_model$homogeneous
  linear <- internal_list$info_model$linear
  additive <- internal_list$info_model$additive

  #############################################
  # PARAMETER LISTS FOR STRUCTURAL COEFFICIENTS
  #############################################

  #------------------------------------------
  # linear homogeneous model
  #------------------------------------------

  if(linear == TRUE &&
     homogeneous == TRUE &&
     parameter_type == "C"){

    ## compute total number of structural coefficients in the model
    ### z-variables onto initial variables
    n_param_C <- internal_list$info_model$n_processes *
      internal_list$info_model$n_time_invariant

    ### z variables onto time-varying NON-initial variables
    n_param_C <- n_param_C +
      sum(lengths(internal_list$info_variables$info_time_invariant_variables)) *
      (internal_list$info_model$n_occasions - 1)

    ### time-varying variables onto time-varying variables
    n_param_C <- n_param_C +
      (internal_list$info_model$n_occasions-1) *
      internal_list$info_model$n_processes^2

    ## create parameter table of structural coefficients
    param_list_C <- matrix(nrow = n_param_C,
                           ncol = 5)
    colnames(param_list_C) <- c("incoming",
                                "outgoing",
                                "label",
                                "constrain",
                                "value")

    param_list_C <- as.data.frame(param_list_C)

    ### z-variables onto initial variables
    #### number of edges
    start_fill <- min(which(is.na(param_list_C[,"incoming"])))
    number_entries <- internal_list$info_model$n_processes *
      internal_list$info_model$n_time_invariant
    end_fill <- start_fill + number_entries - 1

    #### name incoming variable
    param_list_C[start_fill:end_fill,"incoming"] <-
      rep(internal_list$info_variables$user_names_time_varying[,1],
          each = (internal_list$info_model$n_time_invariant))

    #### name outgoing variable
    param_list_C[start_fill:end_fill,"outgoing"] <-
      rep(internal_list$info_variables$names_time_invariant_unique["user_names",],
          internal_list$info_model$n_processes)

    #### constrained
    param_list_C[start_fill:end_fill,"constrain"] <- FALSE

    #### label
    param_list_C[start_fill:end_fill, "label"] <-
      paste0("c_",
             param_list_C[start_fill:end_fill,"incoming"],
             "_",
             param_list_C[start_fill:end_fill,"outgoing"])

    #### value
    param_list_C[start_fill:end_fill, "value"] <-
      param_list_C[start_fill:end_fill, "label"]

    ### z variables onto NON-initial time-varying variables
    #### number of edges
    start_fill <- min(which(is.na(param_list_C[,"incoming"])))
    number_entries <-
      sum(lengths(internal_list$info_variables$info_time_invariant_variables)) *
      (internal_list$info_model$n_occasions - 1)
    end_fill <- start_fill + number_entries - 1

    #### name incoming variable
    name_incoming_variables <-
      vector(mode = "list",
             length = internal_list$info_model$n_processes)

    for (i in 1:internal_list$info_model$n_processes) {
      name_incoming_variables[[i]] <-
        rep(internal_list$info_variables$user_names_time_varying[
          i , (2:internal_list$info_model$n_occasions)],
          each = lengths(
            internal_list$info_variables$info_time_invariant_variables)[i])
    }

    param_list_C[start_fill:end_fill,"incoming"] <-
      as.vector(unlist(name_incoming_variables))

    #### name outgoing variable
    name_outgoing_variables <-
      vector(mode = "list",
             length = internal_list$info_model$n_processes)

    for (i in 1:internal_list$info_model$n_processes) {
      name_outgoing_variables[[i]] <-
        rep(as.vector(internal_list$info_variables$info_time_invariant_variables[[i]]),
            (internal_list$info_model$n_occasions - 1))
    }

    param_list_C[start_fill:end_fill,"outgoing"] <-
      as.vector(unlist(name_outgoing_variables))

    #### constrained
    param_list_C[start_fill:end_fill,"constrain"] <- TRUE

    #### value
    labels_constrained <-
      vector(mode = "list",
             length = internal_list$info_model$n_processes)

    for (i in 1:internal_list$info_model$n_processes){
      labels_constrained[[i]] <-
        paste0("c",
               "_",
               internal_list$info_variables$names_processes["user_names",i],
               "_",
               internal_list$info_variables$info_time_invariant_variables[[i]])
    }

    param_list_C[start_fill:end_fill,"value"] <-
      unlist(
        rep(
          labels_constrained,
          each = (internal_list$info_model$n_occasions - 1)))

    #### label
    param_list_C[start_fill:end_fill, "label"] <-
      paste0("c_",
             param_list_C[start_fill:end_fill,"incoming"],
             "_",
             param_list_C[start_fill:end_fill,"outgoing"])

    ### time-varying variables onto time-varying variables
    #### number of edges
    start_fill <- min(which(is.na(param_list_C[,"incoming"])))
    number_entries <-
      (internal_list$info_model$n_occasions-1) *
      internal_list$info_model$n_processes^2
    end_fill <- start_fill + number_entries - 1

    #### name incoming variable
    name_incoming_variables <-
      vector(mode = "list",
             length = (internal_list$info_model$n_occasions - 1))

    name_incoming_variables_step <-
      vector(mode = "list",
             length = internal_list$info_model$n_processes)

    for (t in 2:internal_list$info_model$n_occasions){
      for (i in 1:internal_list$info_model$n_processes){

        name_incoming_variables_step[[i]] <-
          rep(internal_list$info_variables$user_names_time_varying[i, t],
              internal_list$info_model$n_processes)
      }
      name_incoming_variables[[t-1]] <- name_incoming_variables_step
    }

    param_list_C[start_fill:end_fill,"incoming"] <-
      as.vector(unlist(name_incoming_variables))

    #### name outgoing variable
    name_outgoing_variables <-
      vector(mode = "list",
             length = (internal_list$info_model$n_occasions - 1))

    for (t in 1:(internal_list$info_model$n_occasions - 1)){
      name_outgoing_variables[[t]] <-
        rep(internal_list$info_variables$user_names_time_varying[ , t],
            internal_list$info_model$n_processes)
    }

    param_list_C[start_fill:end_fill,"outgoing"] <-
      as.vector(unlist(name_outgoing_variables))

    #### constrained
    param_list_C[start_fill:end_fill,"constrain"] <- TRUE

    #### value
    labels_constrained <-
      vector(mode = "list",
             length = internal_list$info_model$n_processes)

    for (i in 1:internal_list$info_model$n_processes){
      labels_constrained[[i]] <-
        paste0("c",
               "_",
               internal_list$info_variables$names_processes["user_names",i],
               "_",
               internal_list$info_variables$names_processes["user_names",])
    }

    param_list_C[start_fill:end_fill,"value"] <-
      unlist(
        rep(
          labels_constrained,
          (internal_list$info_model$n_occasions - 1)))

    #### label
    param_list_C[start_fill:end_fill, "label"] <-
      paste0("c_",
             param_list_C[start_fill:end_fill,"incoming"],
             "_",
             param_list_C[start_fill:end_fill,"outgoing"])

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )

    #return output
    return(param_list_C)
  }

  #------------------------------------------
  # linear model with additive heterogeneity
  #------------------------------------------

  if(additive == TRUE &&
     linear == TRUE &&
     homogeneous == FALSE &&
     parameter_type == "C"){

    ## compute total number of structural coefficients in the model
    ### eta-variables onto initial variables
    n_param_C <- internal_list$info_model$n_processes *
      internal_list$info_model$n_processes

    ### z-variables onto initial variables
    n_param_C <- n_param_C +
      internal_list$info_model$n_processes *
      internal_list$info_model$n_time_invariant

    ### eta variables onto time-varying NON-initial variables
    n_param_C <- n_param_C +
      (internal_list$info_model$n_processes *
         (internal_list$info_model$n_occasions - 1))

    ### z variables onto time-varying NON-initial variables
    n_param_C <- n_param_C +
      sum(lengths(internal_list$info_variables$info_time_invariant_variables)) *
      (internal_list$info_model$n_occasions - 1)

    ### time-varying variables onto time-varying variables
    n_param_C <- n_param_C +
      (internal_list$info_model$n_occasions-1) *
      internal_list$info_model$n_processes^2

    ## create parameter table of structural coefficients
    param_list_C <- matrix(nrow = n_param_C,
                           ncol = 5)
    colnames(param_list_C) <- c("incoming",
                                "outgoing",
                                "label",
                                "constrain",
                                "value")

    param_list_C <- as.data.frame(param_list_C)

    ### eta-variables onto initial variables
    #### number of edges
    start_fill <- min(which(is.na(param_list_C[,"incoming"])))
    number_entries <- internal_list$info_model$n_processes *
      internal_list$info_model$n_processes
    end_fill <- start_fill + number_entries - 1

    #### name incoming variable
    param_list_C[start_fill:end_fill,"incoming"] <-
      rep(internal_list$info_variables$user_names_time_varying[,1],
          each = (internal_list$info_model$n_processes))

    #### name outgoing variable
    param_list_C[start_fill:end_fill,"outgoing"] <-
      rep(internal_list$info_variables$names_time_invariant_unobserved[
        "user_names",], internal_list$info_model$n_processes)

    #### constrained
    param_list_C[start_fill:end_fill,"constrain"] <- FALSE

    #### value
    param_list_C[start_fill:end_fill,"value"] <- NA

    #### label
    param_list_C[start_fill:end_fill, "label"] <-
      paste0("c_",
             param_list_C[start_fill:end_fill,"incoming"],
             "_",
             param_list_C[start_fill:end_fill,"outgoing"])

    #### value
    param_list_C[start_fill:end_fill, "value"] <-
      param_list_C[start_fill:end_fill, "label"]

    ### z-variables onto initial variables
    #### number of edges
    start_fill <- min(which(is.na(param_list_C[,"incoming"])))
    number_entries <- internal_list$info_model$n_processes *
      internal_list$info_model$n_time_invariant
    end_fill <- start_fill + number_entries - 1

    #### name incoming variable
    param_list_C[start_fill:end_fill,"incoming"] <-
      rep(internal_list$info_variables$user_names_time_varying[,1],
          each = (internal_list$info_model$n_time_invariant))

    #### name outgoing variable
    param_list_C[start_fill:end_fill,"outgoing"] <-
      rep(internal_list$info_variables$names_time_invariant_unique["user_names",],
          internal_list$info_model$n_processes)

    #### constrained
    param_list_C[start_fill:end_fill,"constrain"] <- FALSE

    #### label
    param_list_C[start_fill:end_fill, "label"] <-
      paste0("c_",
             param_list_C[start_fill:end_fill,"incoming"],
             "_",
             param_list_C[start_fill:end_fill,"outgoing"])

    #### value
    param_list_C[start_fill:end_fill, "value"] <-
      param_list_C[start_fill:end_fill, "label"]

    ### eta variables onto NON-initial time-varying variables
    #### number of edges
    start_fill <- min(which(is.na(param_list_C[,"incoming"])))
    number_entries <- internal_list$info_model$n_processes *
      (internal_list$info_model$n_occasions - 1)
    end_fill <- start_fill + number_entries - 1

    #### name incoming variable
    param_list_C[start_fill:end_fill,"incoming"] <-
      as.vector(
        t(
          internal_list$info_variables$user_names_time_varying[
            , (2:internal_list$info_model$n_occasions)]))

    #### name outgoing variable
    param_list_C[start_fill:end_fill,"outgoing"] <-
      rep(internal_list$info_variables$names_time_invariant_unobserved[
        "user_names",], each = (internal_list$info_model$n_occasions - 1))

    #### constrained
    param_list_C[start_fill:end_fill,"constrain"] <- TRUE

    #### value
    param_list_C[start_fill:end_fill,"value"] <- 1

    #### label
    param_list_C[start_fill:end_fill, "label"] <-
      paste0("c_",
             param_list_C[start_fill:end_fill,"incoming"],
             "_",
             param_list_C[start_fill:end_fill,"outgoing"])

    ### z variables onto NON-initial time-varying variables
    #### number of edges
    start_fill <- min(which(is.na(param_list_C[,"incoming"])))
    number_entries <-
      sum(lengths(internal_list$info_variables$info_time_invariant_variables)) *
      (internal_list$info_model$n_occasions - 1)
    end_fill <- start_fill + number_entries - 1

    #### name incoming variable
    name_incoming_variables <-
      vector(mode = "list",
             length = internal_list$info_model$n_processes)

    for (i in 1:internal_list$info_model$n_processes) {
      name_incoming_variables[[i]] <-
        rep(internal_list$info_variables$user_names_time_varying[
          i , (2:internal_list$info_model$n_occasions)],
          each = lengths(
            internal_list$info_variables$info_time_invariant_variables)[i])
    }

    param_list_C[start_fill:end_fill,"incoming"] <-
      as.vector(unlist(name_incoming_variables))

    #### name outgoing variable
    name_outgoing_variables <-
      vector(mode = "list",
             length = internal_list$info_model$n_processes)

    for (i in 1:internal_list$info_model$n_processes) {
      name_outgoing_variables[[i]] <-
        rep(as.vector(internal_list$info_variables$info_time_invariant_variables[[i]]),
            (internal_list$info_model$n_occasions - 1))
    }

    param_list_C[start_fill:end_fill,"outgoing"] <-
      as.vector(unlist(name_outgoing_variables))

    #### constrained
    param_list_C[start_fill:end_fill,"constrain"] <- TRUE

    #### value
    labels_constrained <-
      vector(mode = "list",
             length = internal_list$info_model$n_processes)

    for (i in 1:internal_list$info_model$n_processes){
      labels_constrained[[i]] <-
        paste0("c",
               "_",
               internal_list$info_variables$names_processes["user_names",i],
               "_",
               internal_list$info_variables$info_time_invariant_variables[[i]])
    }

    param_list_C[start_fill:end_fill,"value"] <-
      unlist(
        rep(
          labels_constrained,
          each = (internal_list$info_model$n_occasions - 1)))

    #### label
    param_list_C[start_fill:end_fill, "label"] <-
      paste0("c_",
             param_list_C[start_fill:end_fill,"incoming"],
             "_",
             param_list_C[start_fill:end_fill,"outgoing"])

    ### time-varying variables onto time-varying variables
    #### number of edges
    start_fill <- min(which(is.na(param_list_C[,"incoming"])))
    number_entries <-
      (internal_list$info_model$n_occasions-1) *
      internal_list$info_model$n_processes^2
    end_fill <- start_fill + number_entries - 1

    #### name incoming variable
    name_incoming_variables <-
      vector(mode = "list",
             length = (internal_list$info_model$n_occasions - 1))

    name_incoming_variables_step <-
      vector(mode = "list",
             length = internal_list$info_model$n_processes)

    for (t in 2:internal_list$info_model$n_occasions){
      for (i in 1:internal_list$info_model$n_processes){

        name_incoming_variables_step[[i]] <-
          rep(internal_list$info_variables$user_names_time_varying[i, t],
              internal_list$info_model$n_processes)
      }
      name_incoming_variables[[t-1]] <- name_incoming_variables_step
    }

    param_list_C[start_fill:end_fill,"incoming"] <-
      as.vector(unlist(name_incoming_variables))

    #### name outgoing variable
    name_outgoing_variables <-
      vector(mode = "list",
             length = (internal_list$info_model$n_occasions - 1))

    for (t in 1:(internal_list$info_model$n_occasions - 1)){
      name_outgoing_variables[[t]] <-
        rep(internal_list$info_variables$user_names_time_varying[ , t],
            internal_list$info_model$n_processes)
    }

    param_list_C[start_fill:end_fill,"outgoing"] <-
      as.vector(unlist(name_outgoing_variables))

    #### constrained
    param_list_C[start_fill:end_fill,"constrain"] <- TRUE

    #### value
    labels_constrained <-
      vector(mode = "list",
             length = internal_list$info_model$n_processes)

    for (i in 1:internal_list$info_model$n_processes){
      labels_constrained[[i]] <-
        paste0("c",
               "_",
               internal_list$info_variables$names_processes["user_names",i],
               "_",
               internal_list$info_variables$names_processes["user_names",])
    }

    param_list_C[start_fill:end_fill,"value"] <-
      unlist(
        rep(
          labels_constrained,
          (internal_list$info_model$n_occasions - 1)))

    #### label
    param_list_C[start_fill:end_fill, "label"] <-
      paste0("c_",
             param_list_C[start_fill:end_fill,"incoming"],
             "_",
             param_list_C[start_fill:end_fill,"outgoing"])

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )

    #return output
    return(param_list_C)

  }

  #############################################
  # PARAMETER LISTS FOR COVARIANCE MATRIX
  #############################################

  #------------------------------------------
  # linear homogeneous model
  #------------------------------------------

  if(linear == TRUE &&
     homogeneous == TRUE &&
     parameter_type == "Psi"){

    Psi <- internal_list$model_matrices$Psi_labels

    # create ordered vector of upper diagonal
    ## z-variables
    start_select <- 1
    end_select <- internal_list$info_model$n_time_invariant
    Psi_z <- Psi[start_select:end_select,start_select:end_select]
    Psi_z_vector <- Psi_z[t(upper.tri(Psi_z, diag = TRUE))]
    Psi_z_vector <- Psi_z_vector[!is.na(Psi_z_vector)]
    Psi_z_vector <- Psi_z_vector[Psi_z_vector != 0]

    ## initial variables
    start_select <- end_select + 1
    end_select <- start_select + internal_list$info_model$n_processes -1
    Psi_init <- Psi[start_select:end_select,start_select:end_select]
    Psi_init_vector <- Psi_init[t(upper.tri(Psi_init, diag = TRUE))]
    Psi_init_vector <- Psi_init_vector[!is.na(Psi_init_vector)]
    Psi_init_vector <- Psi_init_vector[Psi_init_vector != 0]

    ## contemporaneous covariances
    start_select <- end_select + 1
    end_select <- start_select + internal_list$info_model$n_processes -1
    Psi_cont <-
      Psi[start_select:end_select,start_select:end_select]
    Psi_cont_vector <- Psi_cont[t(upper.tri(Psi_cont, diag = TRUE))]
    Psi_cont_vector <- Psi_cont_vector[!is.na(Psi_cont_vector)]
    Psi_cont_vector <- Psi_cont_vector[Psi_cont_vector != 0]
    Psi_cont_vector <- rep(Psi_cont_vector,
                           (internal_list$info_model$n_occasions - 1))

    ## serial covariances
    Psi_serial_vector <- Psi[col(Psi) - row(Psi) ==
                       internal_list$info_model$n_processes]

    Psi_serial_vector <- Psi_serial_vector[
      (max(which(is.na(Psi_serial_vector))) + 1): length(Psi_serial_vector)]

    ## put together the vector

    Psi_vector <-
      c(Psi_z_vector, Psi_init_vector, Psi_cont_vector, Psi_serial_vector)

    n_param_Psi = length(Psi_vector)

    param_list_Psi <- matrix(nrow = n_param_Psi,
                             ncol = 5)

    colnames(param_list_Psi) <- c("incoming",
                                  "outgoing",
                                  "label",
                                  "constrain",
                                  "value")

    param_list_Psi <- as.data.frame(param_list_Psi)

    ## determine rownumbers for serial covariances among noninitial variables
    ## and contemporaneous (co-)variances among noninitial variables
    end_serial <- nrow(param_list_Psi)
    n_serial <- internal_list$info_model$n_processes *
      (internal_list$info_model$n_occasions - 1)
    start_serial <- end_serial - n_serial + 1

    end_cont <- start_serial - 1
    n_cont <- 0.5 * internal_list$info_model$n_processes *
      (internal_list$info_model$n_processes + 1) *
      (internal_list$info_model$n_occasions - 1)
    start_cont <- end_cont - n_cont + 1

    ## fill in column value
    param_list_Psi$value <- Psi_vector

    ## fill in column constrain
    param_list_Psi$constrain <- FALSE
    param_list_Psi$constrain[start_cont:end_cont] <- TRUE


    ## fill in column label
    ### get generic labels of contemporaneous correlations
    Psi_cont_labels <- character(0)
    for (i in 2:internal_list$info_model$n_occasions){
    Psi_cont <-
      matrix(ncol = internal_list$info_model$n_processes,
             nrow = internal_list$info_model$n_processes)

    rownames(Psi_cont) <-
      colnames(Psi_cont) <-
      as.vector(internal_list$info_variables$user_names_time_varying[, i])

    Psi_cont[,] <-
      paste0("psi",
             "_",
             apply(expand.grid(rownames(Psi_cont),
                               colnames(Psi_cont)),
                   1,
                   paste,
                   collapse = "_"))

    Psi_cont[lower.tri(Psi_cont)] <-
      Psi_cont[upper.tri(Psi_cont)]

    Psi_cont_vector <- Psi_cont[t(upper.tri(Psi_cont, diag = TRUE))]
    Psi_cont_vector <- Psi_cont_vector[!is.na(Psi_cont_vector)]
    Psi_cont_vector <- Psi_cont_vector[Psi_cont_vector != 0]

    Psi_cont_labels <- c(Psi_cont_labels, Psi_cont_vector)
    }

    ### replace restricted lables with generic labels and fill in
    Psi_vector[start_cont:end_cont] <- Psi_cont_labels
    param_list_Psi$label <- Psi_vector

    ## fill in column incoming and outgoing
    ### (co-)variances among z-variables
    start_fill <- 1
    number_entries <- internal_list$info_model$n_time_invariant *
      (internal_list$info_model$n_time_invariant + 1) / 2
    end_fill <- start_fill + number_entries -1
    variable_names <-
      internal_list$info_variables$names_time_invariant_unique["user_names",]

    Psi_incoming <- numeric(0)
    Psi_outgoing <- numeric(0)

    for (i in 1:internal_list$info_model$n_time_invariant){
      Psi_incoming <- c(Psi_incoming,
                        (rep(variable_names[i],
                             (internal_list$info_model$n_time_invariant+1-i))))
    }

    for (i in 1:internal_list$info_model$n_time_invariant){
      for (j in i:internal_list$info_model$n_time_invariant){
        Psi_outgoing <- c(Psi_outgoing, variable_names[j])
      }
    }

    param_list_Psi$incoming[start_fill:end_fill] <- Psi_incoming
    param_list_Psi$outgoing[start_fill:end_fill] <- Psi_outgoing

    ### (co-)variances among initial variables
    start_fill <- min(which(is.na(param_list_Psi[,"incoming"])))
    number_entries <- internal_list$info_model$n_processes *
      (internal_list$info_model$n_processes + 1) / 2
    end_fill <- start_fill + number_entries -1
    variable_names <-
      as.character(internal_list$info_variables$user_names_time_varying[,1])

    Psi_incoming <- numeric(0)
    Psi_outgoing <- numeric(0)

    for (i in 1:internal_list$info_model$n_processes){
      Psi_incoming <- c(Psi_incoming,
                        rep(variable_names[i],
                            (internal_list$info_model$n_processes+1-i)))
    }

    for (i in 1:internal_list$info_model$n_processes){
      for (j in i:internal_list$info_model$n_processes){
        Psi_outgoing <- c(Psi_outgoing,variable_names[j])
      }
    }

    param_list_Psi$incoming[start_fill:end_fill] <- Psi_incoming
    param_list_Psi$outgoing[start_fill:end_fill] <- Psi_outgoing

    ### contemporaneous (co-)variances among NON-initial time-varying variables
    start_fill <- min(which(is.na(param_list_Psi[,"incoming"])))
    number_entries <-  0.5 * internal_list$info_model$n_processes *
      (internal_list$info_model$n_processes + 1) *
      (internal_list$info_model$n_occasions - 1)
    end_fill <- start_fill + number_entries -1

    labels_cont_incoming <- character(0)
    labels_cont_outgoing <- character(0)
    for (k in 2:internal_list$info_model$n_occasions){
    variable_names <-
      as.character(internal_list$info_variables$user_names_time_varying[,k])
    Psi_incoming <- numeric(0)
    Psi_outgoing <- numeric(0)

    for (i in 1:internal_list$info_model$n_processes){
      Psi_incoming <- c(Psi_incoming,
                        rep(variable_names[i],
                            (internal_list$info_model$n_processes+1-i)))
    }

    for (i in 1:internal_list$info_model$n_processes){
      for (j in i:internal_list$info_model$n_processes){
        Psi_outgoing <- c(Psi_outgoing,variable_names[j])
      }
    }

    labels_cont_incoming <- c(labels_cont_incoming, Psi_incoming)
    labels_cont_outgoing <- c(labels_cont_outgoing, Psi_outgoing)

    }

    param_list_Psi$incoming[start_fill:end_fill] <- labels_cont_incoming
    param_list_Psi$outgoing[start_fill:end_fill] <- labels_cont_outgoing

    ### serial covariances among NON-initial time-varying variables
    start_fill <- min(which(is.na(param_list_Psi[,"incoming"])))
    number_entries <-  n_serial <- internal_list$info_model$n_processes *
      (internal_list$info_model$n_occasions - 1)
    end_fill <- start_fill + number_entries -1

    labels_serial_incoming <- character(0)
    labels_serial_outgoing <- character(0)

    for (i in 1:(internal_list$info_model$n_occasions - 1)){
      for (j in 1:internal_list$info_model$n_processes){
      entry_labels_serial_incoming <-
        internal_list$info_variables$user_names_time_varying[j,i]
      entry_labels_serial_outgoing <-
        internal_list$info_variables$user_names_time_varying[j,i+1]

      labels_serial_incoming <- c(labels_serial_incoming,
                                  entry_labels_serial_incoming)
      labels_serial_outgoing <- c(labels_serial_outgoing,
                                  entry_labels_serial_outgoing)

      }
      }

    param_list_Psi$incoming[start_fill:end_fill] <- labels_serial_incoming
    param_list_Psi$outgoing[start_fill:end_fill] <- labels_serial_outgoing

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )

    #return output
    return(param_list_Psi)


  }

  #------------------------------------------
  # linear model with additive heterogeneity
  #------------------------------------------

  if(additive == TRUE &&
     linear == TRUE &&
     homogeneous == FALSE &&
     parameter_type == "Psi"){

    Psi <- internal_list$model_matrices$Psi_labels

    Psi_vector <- Psi[t(upper.tri(Psi, diag = TRUE))]

    Psi_vector <- Psi_vector[!is.na(Psi_vector)]

    Psi_vector <- Psi_vector[Psi_vector != 0]

    n_param_Psi = length(Psi_vector)

    param_list_Psi <- matrix(nrow = n_param_Psi,
                             ncol = 5)

    colnames(param_list_Psi) <- c("incoming",
                                  "outgoing",
                                  "label",
                                  "constrain",
                                  "value")

    param_list_Psi <- as.data.frame(param_list_Psi)

    start_time_varying <- nrow(param_list_Psi) -
      ((internal_list$info_model$n_occasions - 1) *
         internal_list$info_model$n_processes) + 1
    end_time_varying <- nrow(param_list_Psi)

    ## fill in column value
    param_list_Psi$value <- Psi_vector

    ## fill in column constrain
    param_list_Psi$constrain <- FALSE
    param_list_Psi$constrain[start_time_varying:end_time_varying] <- TRUE

    ## fill in column label
    Psi_vector[start_time_varying:end_time_varying] <-
      paste0("psi_",
             as.vector(
               internal_list$info_variables$user_names_time_varying[
                 ,
                 2:internal_list$info_model$n_occasions]),
             "_",
             as.vector(
               internal_list$info_variables$user_names_time_varying[
                 ,
                 2:internal_list$info_model$n_occasions]))


    param_list_Psi$label <- Psi_vector

    ## fill in column incoming and outgoing
    ### (co-)variances among eta-variables
    start_fill <- 1
    end_fill <- internal_list$info_model$n_processes *
      (internal_list$info_model$n_processes + 1) / 2
    variable_names <-
    internal_list$info_variables$names_time_invariant_unobserved["user_names",]

    Psi_incoming <- numeric(0)
    Psi_outgoing <- numeric(0)

    for (i in 1:internal_list$info_model$n_processes){
      Psi_incoming <- c(Psi_incoming,
                        rep(variable_names[i],
                            (internal_list$info_model$n_processes+1-i)))
    }

    for (i in 1:internal_list$info_model$n_processes){
      for (j in i:internal_list$info_model$n_processes){
        Psi_outgoing <- c(Psi_outgoing,variable_names[j])
      }
    }

    param_list_Psi$incoming[start_fill:end_fill] <- Psi_incoming
    param_list_Psi$outgoing[start_fill:end_fill] <- Psi_outgoing

    ### (co-)variances among z-variables
    start_fill <- min(which(is.na(param_list_Psi[,"incoming"])))
    number_entries <- internal_list$info_model$n_time_invariant *
      (internal_list$info_model$n_time_invariant + 1) / 2
    end_fill <- start_fill + number_entries -1
    variable_names <-
      internal_list$info_variables$names_time_invariant_unique["user_names",]

    Psi_incoming <- numeric(0)
    Psi_outgoing <- numeric(0)

    for (i in 1:internal_list$info_model$n_time_invariant){
      Psi_incoming <- c(Psi_incoming, (rep(variable_names[i],(internal_list$info_model$n_time_invariant+1-i))))
    }

    for (i in 1:internal_list$info_model$n_time_invariant){
      for (j in i:internal_list$info_model$n_time_invariant){
        Psi_outgoing <- c(Psi_outgoing, variable_names[j])
      }
    }

    param_list_Psi$incoming[start_fill:end_fill] <- Psi_incoming
    param_list_Psi$outgoing[start_fill:end_fill] <- Psi_outgoing

    ### (co-)variances among initial variables
    start_fill <- min(which(is.na(param_list_Psi[,"incoming"])))
    number_entries <- internal_list$info_model$n_processes *
      (internal_list$info_model$n_processes + 1) / 2
    end_fill <- start_fill + number_entries -1
    variable_names <-
      as.character(internal_list$info_variables$user_names_time_varying[,1])

    Psi_incoming <- numeric(0)
    Psi_outgoing <- numeric(0)

    for (i in 1:internal_list$info_model$n_processes){
      Psi_incoming <- c(Psi_incoming,
                        rep(variable_names[i],
                            (internal_list$info_model$n_processes+1-i)))
    }

    for (i in 1:internal_list$info_model$n_processes){
      for (j in i:internal_list$info_model$n_processes){
        Psi_outgoing <- c(Psi_outgoing,variable_names[j])
      }
    }

    param_list_Psi$incoming[start_fill:end_fill] <- Psi_incoming
    param_list_Psi$outgoing[start_fill:end_fill] <- Psi_outgoing

    ### (co-)variances among NON-initial time-varying variables
    start_fill <- min(which(is.na(param_list_Psi[,"incoming"])))
    number_entries <- internal_list$info_model$n_processes *
      (internal_list$info_model$n_occasions - 1)
    end_fill <- start_fill + number_entries -1

    param_list_Psi$incoming[start_fill:end_fill] <-
      param_list_Psi$outgoing[start_fill:end_fill] <-
      as.vector(
        internal_list$info_variables$user_names_time_varying[
          ,
          2:internal_list$info_model$n_occasions])

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )

    #return output
    return(param_list_Psi)

  }
}
