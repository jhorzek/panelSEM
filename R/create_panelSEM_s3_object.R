## Changelog:
# CG 0.0.1 2023-03-24: initial programming

## Documentation
#' @title panelSEM S3 Object
#' @description Assign class \code{panelSEM} to internal list.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return Object of class \code{panelSEM}.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
create_panelSEM_s3_object <- function(internal_list){

	# function name
	fun.name <- "create_panelSEM_s3_object"

	# function version
	fun.version <- "0.0.1 2023-03-24"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# assign class causalSEM
	# CG 0.0.3 2023-03-24: changed name of object of class causalSEM to 
	# internal_list
	
	internal_list <- structure(internal_list, class = "panelSEM")
	
	# console output
	if(verbose >= 2) cat(paste0(" end of function ",
	                            fun.name.version,
	                            " ",
	                            Sys.time(), "\n"))

	# return internal list
	return(internal_list)
}


## development


### test
# require( testthat )
# test_file("../tests/testthat/XXXXXXX.R")
