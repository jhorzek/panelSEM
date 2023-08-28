add_nonlinear_effects <- function(internal_list){

  # If linear == FALSE, we use product terms of
  # observed variables and time invariant predictors
  # to represent the non-linearity
  linear <- internal_list$info_model$linear

  if(linear) # nothing to add
    return(internal_list)

  data          <- internal_list$info_data$data
  observed      <- internal_list$info_variables$user_names_time_varying
  process_names <- internal_list$info_variables$names_processes["user_names",]

  # first, let's add all variance-covariances for the product terms and the
  # time invariant variables.
  product_names <- internal_list$info_data$product_names


}
