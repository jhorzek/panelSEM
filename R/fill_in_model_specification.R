#' @title fill_in_model_specification
#'
#' Creates a lavaan or OpenMx model based in the user specification in internal_list
#' @param internal_list internal list object
#' @return lavaan or OpenMx model
fill_in_model_specification <- function(internal_list){
  if(internal_list$info_model$use_open_mx)
    return(fill_in_model_specification_open_mx(internal_list))

  return(fill_in_model_specification_lavaan(internal_list))
}

#' @title fill_in_model_specification_open_mx
#'
#' Creates an OpenMx model based in the user specification in internal_list
#' @param internal_list internal list object
#' @return OpenMx model
fill_in_model_specification_open_mx <- function(internal_list){
  stop("OpenMx not yet implemented")
}

#' @title fill_in_model_specification_lavaan
#'
#' Creates a lavaan model based in the user specification in internal_list
#' @param internal_list internal list object
#' @return lavaan model
fill_in_model_specification_lavaan <- function(internal_list){
  return(
    specify_model_lavaan(
      data = internal_list$info_data$data,
      labels_time_varying_variables = internal_list$info_variables$user_names_time_varying,
      labels_time_invariant_variables = internal_list$info_variables$names_time_invariant_unique,
      random_intercept = TRUE,
      linear = TRUE,
      verbose = FALSE
      )
  )
}
