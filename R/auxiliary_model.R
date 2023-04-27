## Changelog:
# CG 0.0.1 2023-03-23: initial programming

## Documentation
#' @title Set up auxiliary models
#' @description Set up an auxiliary model from that differs from the target model.
#' @param time_varying_variables List of character vectors containing names of the time-varying
#'  variables. The number entries in the list corresponds to the number of univariate time-series.
#'  Each character vector contains the time-ordered variable names of a univariate time-series
#'  starting with the first measurement occasion.
#' @param time_invariant_variables List of character vectors containing names of the time-invariant
#'  variables. List must have the same length as list in argument \code{time_varying_variables}.
#' @param homogeneous Logical (TRUE / FALSE = default) indicating if the model contains unobserved heterogeneity (TRUE).
#' @param linear Logical (TRUE = default / FALSE) indicating if the model is linear in observed variables (TRUE).
#' @param additive Logical (TRUE = default / FALSE) indicating if the unobserved heterogeneity is additive (TRUE).
#' @param use_open_mx Logical (TRUE / FALSE default) indicating if \code{lavaan} (FALSE) or \code{OpenMx} (TRUE)
#' should be used.
#' @param verbose Integer number describing the verbosity of console output.
#' Admissible values: 0: no output (default), 1: user messages,
#' 2: debugging-relevant messages.
#' @return An object of class \code{panelSEM}.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598
#' @keywords external
#' @export

auxiliary_model <- function(time_varying_variables = NULL,
                            time_invariant_variables = NULL,
                            homogeneous = FALSE,
                            linear = TRUE,
                            additive  = TRUE,
                            use_open_mx = FALSE,
                            verbose = NULL,
                            ...){


  # function name
  fun.name <- "auxiliary_model"

  # function version
  fun.version <- "0.0.1 2023-04-26"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )


  # set verbosity of console output
  verbose <- handle_verbose_argument(verbose = verbose)

  # create empty list
  internal_list <- create_empty_list(verbose = verbose)

  # assign class causalSEM to internal list
  internal_list <- create_panelSEM_s3_object(internal_list = internal_list)

  # print console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, "
                                  ", Sys.time(), "\n" ) )

  # fill in user-specified information about the model into the list
  internal_list <-
    fill_in_info_model(internal_list = internal_list,
                       time_varying_variables = time_varying_variables,
                       time_invariant_variables = time_invariant_variables,
                       homogeneous = homogeneous,
                       linear = linear,
                       additive  = additive,
                       use_open_mx = use_open_mx)

  # fill in model syntax to the list
  internal_list <-
     fill_in_model_specification(internal_list = internal_list)

  auxiliary_model <- internal_list

  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ",
                                  fun.name.version, " ", Sys.time(), "\n" ) )

  # return output
  return(auxiliary_model)

}


### development


## test
