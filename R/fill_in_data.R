## Changelog:
# CG  0.0.1 2023-02-06 initial programming

## Documentation
#' @title fill_in_data
#' @description Fill Data Into List
#' @param data A data.frame containing the data in the wide format.
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

fill_in_data <- function(data = NULL,
                         internal_list = NULL){

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


# console output
if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                Sys.time(), "\n" ) )

# return internal list
return( internal_list )

}
