## Changelog:
# CG 0.0.2 2023-05-24: replaced arguments homogeneity and additive by heterogeneity
# CG 0.0.1 2023-02-20: initial programming

## Documentation
#' @title Get Model Information
#' @description Provide information about the model parameters and model matrices of the dynamic panel data model.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @param time_varying_variables List of character vectors containing names of the time-varying
#'  variables. Within each vector, the variable names must be time-ordered starting with the first
#'  measurement ocassion.
#' @param time_invariant_variables List of character vectors containing names of the time-invariant
#'  variables. List must have the same length as list in argument \code{time_varying_variables}.
#' @param n_occasions Integer number indicating the number of measurement occasions.
#' @param linear Logical (TRUE = default / FALSE) indicating if the model is linear in observed variables (TRUE).
#' @param heterogeneity Character vector indicating the type of unobserved heterogeneity. Admissible values are \code{"homogeneous"}, \code{"additive"}, \code{"autoregressive"}, and \code{"cross-lagged"} (or any non-conflicting combination).
#' @param use_open_mx Logical (TRUE / FALSE default) indicating if \code{lavaan} (FALSE) or \code{OpenMx} (TRUE)
#' should be used.
#' @return The inputted internal_list with several slots filled in:
#' \tabular{lll}{
#'  \code{..$info_parameters$C_table}: \code{data.frame}  \tab \tab Number of measurement occasions. \cr
#'  \code{..$info_parameters$Psi_table}: \code{data.frame}  \tab \tab Number of dynamic processes. \cr
#'  \code{..$model_matrices$C_labels}: \code{char([0,0])} \tab \tab Table with generic variable names of time-varying variables.\cr
#'  \code{..$model_matrices$Psi_labels}: \code{char([0,0])} \tab \tab Table with user-specified variable names of time-varying variables.\cr
#'  \code{..$model_matrices$select_observed_only}: \code{num([0,0])} \tab \tab Table with generic variable names of time-invariant variables.\cr
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598


# function definition
fill_in_info_model <- function(internal_list = NULL,
                               time_varying_variables = NULL,
                               time_invariant_variables = NULL,
                               linear = TRUE,
                               heterogeneity  = NULL,
                               use_open_mx = FALSE){

  # function name
  fun.name <- "fill_in_info_model"

  # function version
  fun.version <- "0.0.1 2023-02-20"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )

  # TODO: Argument checks

  ##############################################
  ###### Parameter Tables and Model Matrices
  ##############################################

  # fill parameter list of structural coefficients into internal_list
  internal_list$info_parameters$C_table <-
    create_parameter_table(internal_list = internal_list,
                           parameter_type = "C")

  # fill in matrix of structural coefficients to internal list
  internal_list$model_matrices$C_labels <-
    create_model_matrix(internal_list = internal_list,
                        matrix_type = "C")

  # fill covariance matrix into internal_list
  internal_list$model_matrices$Psi_labels <-
    create_model_matrix(internal_list = internal_list,
                        matrix_type = "Psi")

  # fill in parameter list of covariance parameters into internal_list
  internal_list$info_parameters$Psi_table <-
    create_parameter_table(internal_list = internal_list,
                           parameter_type = "Psi")

  #############################################################
  # FILTER / SELECTION MATRIX TO SELECT OBSERVED VARIABLES ONLY
  #############################################################

  internal_list$model_matrices$select_observed_only <-
    create_model_matrix(internal_list = internal_list,
                        matrix_type = "selection")

  ####################################
  # TODO: find a way to capture all possible combinations of elements
  # from {"additive", "autoregressive" , "cross-lagged"}

  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )

  # return internal list
  return( internal_list )
}
