## Changelog:
# CG 0.0.1 2023-03-23: initial programming

## Documentation
#' @title Add Starting Values to Model
#' @description Add starting values to an existing model.
#' @param internal_list A list with various information extracted from the
#'    model.
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

add_starting_values <- function(internal_list){

  # function name
  fun.name <- "add_starting_values"

  # function version
  fun.version <- "0.0.1 2023-08-09"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  # get verbose argument
  verbose <- internal_list$control$verbose

  # print console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, "
                                  ", Sys.time(), "\n" ) )

  if (!internal_list$info_model$use_open_mx){

    starting_values_C <-c(internal_list$info_parameters$C_table$start_1)
    names(starting_values_C) <- internal_list$info_parameters$C_table$value
    starting_values_C <- starting_values_C[!is.na(starting_values_C)]
    starting_values_C <- starting_values_C[unique(names(starting_values_C))]

    starting_values_Psi <-c(internal_list$info_parameters$Psi_table$start_1)
    names(starting_values_Psi) <- internal_list$info_parameters$Psi_table$value
    starting_values_Psi <- starting_values_Psi[!is.na(starting_values_Psi)]
    starting_values_Psi <- starting_values_Psi[unique(names(starting_values_Psi))]

    starting_values <- c(starting_values_C, starting_values_Psi)

    model_syntax <- internal_list$model_syntax$lavaan
    parameter_table <- lavaan::lavaanify(model = model_syntax)

    parameter_table_with_start <- set_starting_values_lavaan(parameter_table,
                                                             starting_values)

    internal_list$model_syntax$lavaan <- parameter_table_with_start
  }

  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ",
                                  fun.name.version, " ", Sys.time(), "\n" ) )

  # return output
  return(internal_list)

}

set_starting_values_lavaan <- function(parameter_table, starting_values){
  if(is.null(names(starting_values)) | !is.numeric(starting_values))
    stop("starting_values must be a numeric vector with names")

  # change labels if none are given
  parameter_table$label[parameter_table$label == ""] <- paste0(parameter_table$lhs,
                                                               parameter_table$op,
                                                               parameter_table$rhs)[parameter_table$label == ""]

  for(i in 1:length(starting_values)){
    current_label <- names(starting_values)[i]
    current_value <- starting_values[i]

    if(!current_label %in% parameter_table$label)
      stop("Could not find the following parameter in the model: ", current_label)

    parameter_table$ustart[parameter_table$label == current_label] <- current_value

  }

  return(parameter_table)
}
