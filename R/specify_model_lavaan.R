## Documentation
#' @title specify_model_lavaan
#' @description creates a lavaan model syntax based on data in wide format
#' @param data data set in wide format
#' @param labels_time_varying_variables vector with labels of time - varying variables
#' @param labels_time_invariant_variables list with vectors holding labels of time - invariant variables
#' @param random_intercept set to TRUE to add a random intercept to the model
#' @param linear set to TRUE to get the syntax for a linear model and to FALSE
#' for a non-linear model. Currently only TRUE is supported.
#' @param verbose set to TRUE to get more feedback
#' @return lavaan model syntax
#' @examples
#' library(panelSEM)
#'
#' set.seed(23)
#'
#' time_points <- 10
#'
#' ############################################
#' # set values of population parameters SET 1
#' ############################################
#'
#'
#' # covariance-matrix of the latent traits
#' A_sigma_eta <- matrix(nrow = 2, ncol = 2, c(1, 0.5, 0.5, 1))
#' sigma_epsilon_eta <- t(A_sigma_eta) %*% A_sigma_eta
#'
#' # covariance-matrix of the epsilon_z variables
#' A_sigma_z <- matrix(nrow = 3, ncol = 3, c(1, 0.75, 0.3, 0.75, 1, 0.25,
#'                                           0.3, 0.25, 1))
#' sigma_epsilon_z <- t(A_sigma_z) %*% A_sigma_z
#'
#' # covariance matrix of the initial residuals
#' A_sigma_eps_init <- matrix(nrow = 2, ncol = 2, c(1, 2, 2, 1))
#' sigma_epsilon_init <- t(A_sigma_eps_init) %*% A_sigma_eps_init
#'
#' population_parameters <- data.frame(
#'   N = NA,
#'   # directed effects
#'   ## latent traits
#'   c_x1_etax = -4,
#'   c_x1_etay = 8,
#'
#'   c_y1_etax = -12,
#'   c_y1_etay = 19,
#'
#'   c_x_etax = 1,
#'   c_y_etay = 1,
#'
#'   ## time independent predictors
#'   c_x1_z1 = -5,
#'   c_x1_z2 = -1,
#'   c_x1_z3 = 4,
#'
#'   c_y1_z1 = - 8,
#'   c_y1_z2 = 2,
#'   c_y1_z3 = 6,
#'
#'   c_x_z1 = 0.5,
#'   c_x_z2 = 2,
#'
#'   c_y_z2 = 1.5,
#'   c_y_z3 = 2,
#'
#'   ## autoregressive and cross-lagged effects
#'   c_x_x = 0.05,
#'   c_x_y = 0.4,
#'   c_y_x = -0.6,
#'   c_y_y = 1.2,
#'
#'   # undirected effects
#'   ## trait
#'   psi_etax_etax = sigma_epsilon_eta[1,1],
#'   psi_etax_etay = sigma_epsilon_eta[1,2],
#'   psi_etay_etay = sigma_epsilon_eta[2,2],
#'
#'   ## observed predictors
#'   psi_z1_z1 = sigma_epsilon_z[1,1],
#'   psi_z1_z2 = sigma_epsilon_z[1,2],
#'   psi_z1_z3 = sigma_epsilon_z[1,3],
#'   psi_z2_z2 = sigma_epsilon_z[2,2],
#'   psi_z2_z3 = sigma_epsilon_z[2,3],
#'   psi_z3_z3 = sigma_epsilon_z[3,3],
#'
#'   ## residuals
#'   ### initial time point
#'   psi_x1_x1 = sigma_epsilon_init[1,1],
#'   psi_x1_y1 = sigma_epsilon_init[1,2],
#'   psi_y1_y1 = sigma_epsilon_init[2,2],
#'
#'   ### subsequent time points
#'   psi_x_x = 1,
#'   psi_y_y = 1
#' )
#'
#' population_parameters$N <- 1000
#'
#' population_parameters$time_points <- time_points
#'
#' data <- do.call(what = simulate_data,
#'                 args = population_parameters)
#'
#' model_syntax <- specify_model_lavaan(data = data,
#'                               labels_time_varying_variables = list(paste0("x", 1:time_points),
#'                                                                    paste0("y", 1:time_points)),
#'                               labels_time_invariant_variables = list(c("z1", "z2"),
#'                                                                      c("z2", "z3")))
#'
#' library(lavaan)
#'
#' model_fit <- lavaan(model = model_syntax,
#'                     data = data)
#' coef(model_fit)
specify_model_lavaan <- function(data,
                                 labels_time_varying_variables,
                                 labels_time_invariant_variables = NULL,
                                 random_intercept = TRUE,
                                 linear = TRUE,
                                 verbose = FALSE){

  # function name
  fun.name <- "specify_model"

  # function version
  fun.version <- "0.0.1 2023-03-07"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  # console output
  if( verbose )
    cat( paste0( "start of function ", fun.name.version,
                 " ", Sys.time(), "\n" ) )


  .check_input(data,
               labels_time_varying_variables,
               labels_time_invariant_variables)

  # get basic information about specification
  n_time_points <- labels_time_varying_variables |>
    sapply(length) |>
    unique()

  unique_times <- 1:n_time_points

  n_processes <- length(labels_time_varying_variables)

  # to make the rest easier, we will initialize a list with labels_time_invariant_variables,
  # if none was provided
  if(is.null(labels_time_invariant_variables)){
    labels_time_invariant_variables <- vector("list", length(time_varying_variable_base))
  }

  # initialize syntax
  syntax <-
    "
########################
# Loadings
########################
"

  # Add random intercept

  if(random_intercept){
    random_intercept_labels <- c()
    for(tvv in 1:n_processes){

      random_intercept_label <- paste0("eta", tvv)

      random_intercept_labels <- c(random_intercept_labels,
                                   random_intercept_label)

      # for simplicity, we will first find out the names of the variables
      # at the initial time point as well as all other time points:

      # for the processes we are currently interested in:
      tvv_initial <- labels_time_varying_variables[[tvv]][1]

      tvv_other <- labels_time_varying_variables[[tvv]][-1]

      # and all other processes
      non_tvv_initial <- labels_time_varying_variables[-tvv] |>
        sapply(function(x) x[1])

      syntax <- c(
        syntax,
        paste0(
          random_intercept_label," =~ ",
          # initial time point
          "c_", tvv, unique_times[1],"_", random_intercept_label,
          " * ", tvv_initial, " + ",
          # subsequent time points
          paste0(paste0("1 * ", tvv_other), collapse = " + "),
          " + ",
          # other variables at initial time point
          paste0(
            paste0(
              paste0("c_",
                     non_tvv_initial,
                     "_", random_intercept_label
              ), "*", non_tvv_initial),
            collapse = " + "
          )
        )
      )

    }
  }

  syntax <- c(syntax,
              "

########################
# regressions
########################
")

  labels_time_invariant_variables_combined <- labels_time_invariant_variables |>
    unlist() |>
    unique()

  has_ti_predictors <- !is.null(labels_time_invariant_variables_combined)

  for(tp in unique_times){

    for(tvv in 1:n_processes){

      # for the processes we are currently interested in:
      tvv_label <- labels_time_varying_variables[[tvv]][tp]

      ## Step 1: Add time-invariant predictors:
      ### For the initial time point:
      if((tp == min(unique_times)) & has_ti_predictors){

        # add prediction of initial time point using all time independent predictors
        syntax <- c(syntax,
                    paste0(tvv_label, " ~ ", paste0(
                      sapply(labels_time_invariant_variables_combined, function (x) paste0(
                        "c_", tvv_label, "_", x, " * ", x
                      )), collapse = " + "))
        )

        # skip rest as there are no autoregressive or cross-lagged effects
        # to add
        next

      }else if(tp == min(unique_times)){
        # if there is no time independent predictors, we can skip initial time points
        # as there are no autoregressive or cross-lagged effects to add
        next
      }

      ### For all subsequent time points:
      if(!is.null(labels_time_invariant_variables[[tvv]])){
        # if the current variable is predicted by the time invariant predictors:
        cross_lagged <- paste0(tvv_label, " ~ ", paste0(
          sapply(labels_time_invariant_variables[[tvv]], function (x) paste0(
            "c_", tvv, "_", x, " * ", x
          )), collapse = " + "))

      }else{
        # if it is not predicted, we can just start with the regressions for
        # the cross-lagged panel model
        cross_lagged <- paste0(tvv_label, " ~ ")
      }

      # Step 2: Add autoregressive and cross-lagged parameters

      tvv_previous <- labels_time_varying_variables |>
        sapply(function(x) x[tp-1])

      syntax <- c(
        syntax,
        paste0(
          cross_lagged,
          " + ",
          paste0(
            paste0("c_", tvv, "_", 1:n_processes,
                   " * ",
                   tvv_previous
            ), collapse = " + "
          )
        )
      )

    }
  }

  syntax <- c(syntax,
              "

####################################
# variances and covariances
####################################
")

  # random intercepts
  if(random_intercept){
    for(i in 1:length(random_intercept_labels)){
      syntax <- c(syntax,
                  paste0(random_intercept_labels[i], " ~~ ",
                         paste0(
                           paste0(
                             paste0("psi_", random_intercept_labels[i], "_", random_intercept_labels[i:length(random_intercept_labels)]),
                             " * ", random_intercept_labels[i:length(random_intercept_labels)]),
                           collapse = " + "
                         )
                  )
      )
    }
  }

  # dynamic residuals

  # add unique variances and covariances for the initial time points
  for(i in 1:n_processes){
    tvv <- labels_time_varying_variables[[i]][1]
    # other variables
    cov_with <- c()
    for(j in i:n_processes){
      cov_with <- c(cov_with,
                    labels_time_varying_variables[[j]][1]
      )
    }

    syntax <- c(syntax,
                paste0(tvv, " ~~ ",
                       paste0(
                         paste0(
                           paste0("psi_", tvv, "_",
                                  cov_with),
                           " * ", cov_with),
                         collapse = " + "
                       )
                )
    )
  }

  # add subsequent time points

  for(tp in unique_times){
    if(tp == min(unique_times))
      next

    for(i in 1:n_processes){
      tvv <- labels_time_varying_variables[[i]][tp]

      syntax <- c(syntax,
                  paste0(tvv, " ~~ psi_", i, "_", i, " * ", tvv)
      )
    }
  }


  return(paste0(syntax, collapse = "\n"))

}

#' @title .check_input
#' @description check input to specify_model function
#' @param data data set in wide format
#' @param labels_time_varying_variables vector with labels of time - varying variables
#' @param labels_time_invariant_variables list with vectors holding labels of time - invariant variables
#' @return throws error
.check_input <- function(data,
                         labels_time_varying_variables,
                         labels_time_invariant_variables){

  # check class
  if(!is(labels_time_varying_variables, "list"))
    stop("labels_time_varying_variables must be a list")

  # Check if all specified labels can be found in the column names of the
  # data set
  if(any(! unlist(labels_time_varying_variables) %in% colnames(data)))
    stop("Could not find the following time varying variables in the ",
         "data set: ",
         paste0(
           unlist(labels_time_varying_variables)[!unlist(labels_time_varying_variables) %in% colnames(data)],
           collapse =", ")
    )

  # check if all processes have the same number of measurement occasions
  lengths <- labels_time_varying_variables |>
    sapply(length) |>
    unique()

  if(length(lengths) != 1)
    stop("Some time varying variables appear more often in the data set ",
         "than others. Please make sure that all variables have been observed ",
         "at all measurement occasions. If this was not the case, ",
         "add a column with NAs for the respective variables.")

  ## Check the time invariant predictors:

  if(is.null(labels_time_invariant_variables))
    return() # nothing to check

  if(!is.list(labels_time_invariant_variables))
    stop("labels_time_invariant_variables must be a list")

  if(length(labels_time_invariant_variables) != length(labels_time_varying_variables))
    stop("labels_time_invariant_variables must have the same length as labels_time_varying_variables.",
         "If one or multiple of your processes don't have time invariant predictors,",
         "set the respective list entries to NULL (e.g., list(c('z1','z2'), NULL))")

  for(i in 1:length(labels_time_invariant_variables)){
    if(!all(labels_time_invariant_variables[[i]] %in% colnames(data))){
      stop("Could not find the following time invariant variables in the ",
           "data set: ",
           paste0(
             labels_time_invariant_variables[[i]][!labels_time_invariant_variables[[i]] %in% colnames(data)],
             collapse =", ")
      )
    }
  }
}

#' @title .get_time_varying_variable_base
#' @description returns just the names of time-varying variables without the time indices
#' @param labels_time_varying_variables vector with labels of time - varying variables
#' @return vector with labels
.get_time_varying_variable_base <- function(
    labels_time_varying_variables
){
  return(
    gsub(pattern = "[0-9]+$",
         replacement = "",
         x = labels_time_varying_variables) |>
      unique()
  )
}

#' @title .get_time_varying_variable_base
#' @description returns just the times of time-varying variables without the labels
#' @param labels_time_varying_variables vector with labels of time - varying variables
#' @return vector with numbers
.get_time_varying_variable_time <- function(
    labels_time_varying_variables
){
  parameters <- gsub(pattern = "[0-9]+$",
                     replacement = "",
                     x = labels_time_varying_variables)
  time_points <- stringr::str_extract(string = labels_time_varying_variables,
                                      "[0-9]+$") |>
    as.numeric()

  names(time_points) <- parameters
  return(
    time_points
  )
}


