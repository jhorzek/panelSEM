## Changelog:
# CG 0.0.1 2023-03-23: initial programming

## Documentation
#' @title Fit Dynamic Panel Data Models 
#' @description Fit a model from the class of dynamic panel data models to longitudinal data. 
#' Models include linear and nonlinear cross-lagged panel models with additive or nonadditive
#' unobserved heterogeneity. 
#' @param data A \code{data.frame} containing the data with named columns. Data MUST be in wide format.
#' @param time_varying_variables List of character vectors containing names of the time-varying
#'  variables. The number entries in the list corresponds to the number of univariate time-series.
#'  Each character vector contains the time-ordered variable names of a univariate time-series 
#'  starting with the first measurement occasion. 
#' @param time_invariant_variables List of character vectors containing names of the time-invariant
#'  variables. List must have the same length as list in argument \code{time_varying_variables}. 
#' @param homogeneous Logical (TRUE / FALSE = default) indicating if the model contains unobserved heterogeneity (TRUE). 
#' @param linear Logical (TRUE = default / FALSE) indicating if the model is linear in observed variables (TRUE). 
#' @param additive Logical (TRUE = default / FALSE) indicating if the unobserved heterogeneity is additive (TRUE).  
#' @param use_resamples Logical (TRUE / FALSE = default) indicating if a resampling procedure is used for the computation of starting values and model diagnostics.  
#' @param use_open_mx Logical (TRUE / FALSE default) indicating if \code{lavaan} (FALSE) or \code{OpenMx} (TRUE) 
#' should be used. 
#' @param verbose Integer number describing the verbosity of console output.
#' Admissible values: 0: no output (default), 1: user messages, 
#' 2: debugging-relevant messages.
#' @return An object of class \code{panelSEM} for which several methods 
#' are available including \code{\link{summary.panelSEM}} and 
#' \code{\link{print.panelSEM}}.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural 
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492, 
#'  DOI: 10.1080/10705511.2020.1780598 
#' @keywords external
#' @export

fit_panel_sem <- function(data = NULL,
                          time_varying_variables = NULL,
                          time_invariant_variables = NULL,
                          homogeneous = FALSE,
                          linear = TRUE,
                          additive  = TRUE,
                          use_resamples = FALSE,
                          use_open_mx = FALSE,
                          verbose = NULL,
                          ...){


  # function name
  fun.name <- "fit_panel_sem"

  # function version
  fun.version <- "0.0.1 2023-02-20"

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

  # fill in user-specified data to the list
  internal_list <- fill_in_data( internal_list = internal_list,
                                 data = data )
  
  # fill in user-specified information about the model into the list
  internal_list <- 
    fill_in_info_model(internal_list = internal_list,
                       time_varying_variables = time_varying_variables,
                       time_invariant_variables = time_invariant_variables,
                       homogeneous = homogeneous,
                       linear = linear,
                       additive  = additive)

  # fill in model syntax to the list
  # internal_list <-
  #  fill_in_model_specification(internal_list = internal_list)
  
  # TODO: decide on default settings for (i) when to include resampling and 
  # (ii) the default settings of the resampling procedure 
  # if (use_resamples == TRUE){
    
    # fill in estimation results to the list
  #  internal_list <- fill_in_resample(internal_list = internal_list)
    
  #}
  
  # fill in estimation results to the list
  #internal_list <- fill_in_estimates(internal_list = internal_list)
  
  # fill in diagnostic results into the list
  #internal_list <- fill_in_diagnostics(internal_list = internal_list)
  
  # fill in warning messages into the list
  #internal_list <- fill_in_warnings(internal_list = internal_list)

  # fill in outputs for generic functions to the list
  #internal_list$tables <- fill_in_print_table(internal_list = internal_list)

  # prepare output
  panelSEM_object <- internal_list
  
  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", 
                                  fun.name.version, " ", Sys.time(), "\n" ) )

  # return output
  return(panelSEM_object)

  }


### development


## test 


