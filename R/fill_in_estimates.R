## Changelog:
# CG 0.0.1 2023-03-23: initial programming

## Documentation
#' @title Fit Panel Data Model
#' @description Fit a model from the class of dynamic panel data models to longitudinal data.
#' Models include linear and nonlinear cross-lagged panel models with additive or nonadditive
#' unobserved heterogeneity.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @param starting_values Character string indicating which type of starting values should be used. Admissible values are \code{"default"}, \code{"model-based"}, or \code{"random"}.
#' @return The inputted list with several slots in \code{..$info_data} filled
#' in.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598
#' @keywords external

fill_in_estimates <- function(internal_list,
                              starting_values){

  # function name
  fun.name <- "fill_in_estimates"

  # function version
  fun.version <- "0.0.1 2023-03-23"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  # get verbose argument
  verbose <- internal_list$control$verbose

  # print console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, "
                                  ", Sys.time(), "\n" ) )

  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ",
                                  fun.name.version, " ", Sys.time(), "\n" ) )

  # return output
  return(internal_list)

}
