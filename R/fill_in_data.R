## Changelog:
# CG  0.0.3 2023-06-06:  changed labels of product terms to "prod_x_y"
# CG  0.0.2 2023-05-24: replaced arguments homogeneity and additive by heterogeneity
#						added argument add_product_variables and corresponding section
# CG  0.0.1 2023-02-06 initial programming

## Documentation
#' @title fill_in_data
#' @description Fill Data Into List
#' @param data A data.frame containing the data in the wide format.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @param add_product_variables Logical value indicating if (\code{TRUE})
#' product terms of observed variables should be added to the
#' \code{..$info_data} slot.
#' @return The inputted list with several slots in \code{..$info_data} filled
#' in.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598

fill_in_data <- function(data,
                         internal_list,
                         add_product_variables){

# function name
fun.name <- "fill_in_data"

# function version
fun.version <- "0.0.1 2023-02-06"

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

internal_list$info_data$data <- data
internal_list$info_data$n_obs <- nrow(data)
internal_list$info_data$n_var <- ncol(data)
internal_list$info_data$var_names <- colnames(data)

# add product terms of observed variables models are nonlinear
if(add_product_variables == TRUE){

 product_terms_names <- character(0)

 for (i in 1:internal_list$info_model$n_processes){
   names_product <-
   as.vector(outer(
   internal_list$info_variables$info_time_invariant_variables[[i]],
   internal_list$info_variables$user_names_time_varying[
     i,
     -internal_list$info_model$n_occasions],
   paste,
   sep="_"))

   product_terms_names <- c(product_terms_names, names_product)
 }

 product_terms_names <- paste0("prod_",product_terms_names)
 product_final <- numeric(0)

   for (i in 1:internal_list$info_model$n_processes){
       mat <- internal_list$info_variables$info_time_invariant_variables[[i]]
       vec <- internal_list$info_variables$user_names_time_varying[
         i,
         -internal_list$info_model$n_occasions]

       for (j in 1:(internal_list$info_model$n_occasions - 1)){
         product <- sweep(as.matrix(data[, mat]),
                          MARGIN=1,
                          as.numeric(data[, vec[j]]),
                          `*`)

         product_final <- cbind(product_final, product)
       }
       }

   data_product_terms <- as.data.frame(product_final)
   colnames(data_product_terms) <- product_terms_names

   internal_list$info_data$data_product_terms <- data_product_terms

}

# console output
if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                Sys.time(), "\n" ) )

# return internal list
return( internal_list )

}
