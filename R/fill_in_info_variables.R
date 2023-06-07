## Changelog:
# CG 0.0.3 2023-06-05: split function into fill_in_info_variables
# CG 0.0.2 2023-05-24: replaced arguments homogeneity and additive by heterogeneity
# CG 0.0.1 2023-02-20: initial programming

## Documentation
#' @title Get Information about Variables
#' @description Provide information about the variables in the dynamic panel data model.
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
#'  \code{..$n_ocassions}: \code{int(0)}  \tab \tab Number of measurement occasions. \cr
#'  \code{..$n_processes}: \code{int(0)}  \tab \tab Number of dynamic processes. \cr
#'  \code{..$generic_names_time_varying}: \code{data.frame} \tab \tab Table with generic variable names of time-varying variables.\cr
#'  \code{..$user_names_time_varying}: \code{data.frame} \tab \tab Table with user-specified variable names of time-varying variables.\cr
#'  \code{..$generic_names_time_invariant}: \code{data.frame} \tab \tab Table with generic variable names of time-invariant variables.\cr
#'  \code{..$user_names_time_invariant}: \code{data.frame} \tab \tab Table with user-specified variable names of time-invariant variables.}
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598


# function definition
fill_in_info_variables <- function(internal_list,
                                   time_varying_variables,
                                   time_invariant_variables,
                                   linear,
                                   heterogeneity,
                                   use_open_mx){

	# function name
	fun.name <- "fill_in_info_variables"

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
	# - give warning if homogeneous and additive are both set to conflicting values
	# TODO: DELETE THIS LINE: internal_list <- vector(mode = "list")

	###########################
	# GENERAL MODEL INFORMATION
	###########################
	## extract model information from the arguments
	internal_list$info_model$n_occasions <-
	  length(time_varying_variables[[1]])
	internal_list$info_model$n_processes <-
	  sum(lengths(time_varying_variables)) /
	  internal_list$info_model$n_occasions
	internal_list$info_model$n_time_invariant <-
	  length(unique(unlist(time_invariant_variables)))
	internal_list$info_model$linear <- linear
	internal_list$info_model$heterogeneity  <- heterogeneity
	internal_list$info_model$use_open_mx <- use_open_mx

  ####################
	# OBSERVED VARIABLES
	####################
	## time-varying variables
	### generic names time-varying variables
	table_generic_names_colnames <-
	  as.character(1:internal_list$info_model$n_occasions)
	table_generic_names_rownames <-
	  paste0("process_",LETTERS[1:internal_list$info_model$n_processes])

	table_generic_names <-
	  matrix(
	    paste0(rep(gsub("process_", "",table_generic_names_rownames),
	               each = internal_list$info_model$n_occasions),
	           rep(table_generic_names_colnames,
	               internal_list$info_model$n_processes)),
	    byrow = TRUE,
	    ncol = internal_list$info_model$n_occasions,
	    nrow = internal_list$info_model$n_processes)

	rownames(table_generic_names) <- table_generic_names_rownames
	colnames(table_generic_names) <- table_generic_names_colnames

	internal_list$info_variables$generic_names_time_varying <-
	  table_generic_names

	### user-specified names time-varying variables
	table_user_names <- matrix(ncol = internal_list$info_model$n_occasions,
	                            nrow = internal_list$info_model$n_processes)

	for (i in 1:internal_list$info_model$n_processes){
	table_user_names[i,] <- time_varying_variables[[i]]
	}

	rownames(table_user_names) <- table_generic_names_rownames
	colnames(table_user_names) <- table_generic_names_colnames

	internal_list$info_variables$user_names_time_varying <-
	  table_user_names

	### names of processes
	user_names_processes <-
	  vector("character", length = internal_list$info_model$n_processes)

	for (i in 1:internal_list$info_model$n_processes){
	  user_names_processes[i] <-
	    Reduce(intersect, strsplit(
	      gsub('[[:digit:]]+',
	           '',
	           internal_list$info_variables$user_names_time_varying[i,]),
	      "[^a-zA-Z0-9-]" ))
	}

	# TODO: use abbreviate function?
	# do not remove number
	# check if labels are empty set (do structure split after three characters)
	# check if labels are still unique
	# print labels of processes in summary output?
	# if nothing works produce error and ask user to provide process labels in argument

	table_names_processes <-
	  matrix(ncol = internal_list$info_model$n_processes,
	         nrow = 2)

	table_names_processes[1,] <- user_names_processes
	table_names_processes[2,] <-
	  gsub("process_",
	       "",
	       rownames(internal_list$info_variables$user_names_time_varying))

	rownames(table_names_processes) <- c("user_names","generic_names")

	internal_list$info_variables$names_processes <-
	  table_names_processes

	## time-invariant variables
	user_names_time_invariant_variables <- time_invariant_variables
	names(user_names_time_invariant_variables) <-
	  rownames(table_user_names)

	internal_list$info_variables$info_time_invariant_variables <-
	  user_names_time_invariant_variables

	table_time_invariant <-
	  matrix(ncol = internal_list$info_model$n_time_invariant,
	         nrow = 2)

	table_time_invariant[1,] <- unique(unlist(time_invariant_variables))
	table_time_invariant[2,] <-
	  paste0("z",1:internal_list$info_model$n_time_invariant)

	rownames(table_time_invariant) <- c("user_names","generic_names")

	internal_list$info_variables$names_time_invariant_unique <-
	  table_time_invariant

	######################
	# UNOBSERVED VARIABLES
  ######################
	# TODO: fill in the structural error terms if desired. These are
	# the error terms (epsilon-terms) of the time-varying and time-invariant
	# variables

	#------------------------------------------
	# additive random coefficients
	#------------------------------------------

	if("additive" %in% heterogeneity){

	## time-invariant variables
	time_invariant_variables_unobserved_user_names <-
	paste0("eta", user_names_processes)

	time_invariant_variables_unobserved_generic_names <-
	paste0("eta",
	       gsub("process_",
	            "",
	            rownames(internal_list$info_variables$user_names_time_varying)))

	table_user_names_processes <-
	  matrix(ncol = internal_list$info_model$n_processes,
	         nrow = 2)

	table_user_names_processes[1,] <-
	  time_invariant_variables_unobserved_user_names
	table_user_names_processes[2,] <-
	  time_invariant_variables_unobserved_generic_names

	rownames(table_user_names_processes) <- c("user_names","generic_names")

	internal_list$info_variables$names_time_invariant_unobserved_additive <-
	  table_user_names_processes
	}

	#------------------------------------------
	# autoregressive random coefficients
	#------------------------------------------

	if("autoregressive" %in% heterogeneity){

	  ## time-invariant variables user names

	  auto_regressive <-
	    matrix(ncol = internal_list$info_model$n_processes,
	           nrow = internal_list$info_model$n_processes)

	  rownames(auto_regressive) <- colnames(auto_regressive) <-
	    user_names_processes

	  auto_regressive[,] <-
	    paste0("eta",
	           apply(expand.grid(rownames(auto_regressive),
	                             colnames(auto_regressive)),
	                 1,
	                 paste,
	                 collapse = ""))

	  auto_regressive_user <- character(0)

	  for (i in 1:internal_list$info_model$n_processes){

	    auto_regressive_add <- auto_regressive[i, i]
	    auto_regressive_user <- c(auto_regressive_user, auto_regressive_add)

	  }

	  ## time-invariant variables generic names
	  auto_regressive <-
	    matrix(ncol = internal_list$info_model$n_processes,
	           nrow = internal_list$info_model$n_processes)

	  rownames(auto_regressive) <- colnames(auto_regressive) <-
	    internal_list$info_variables$names_processes["generic_names", ]

	  auto_regressive[,] <-
	    paste0("eta",
	           apply(expand.grid(rownames(auto_regressive),
	                             colnames(auto_regressive)),
	                 1,
	                 paste,
	                 collapse = ""))

	  auto_regressive_generic <- character(0)

	  for (i in 1:internal_list$info_model$n_processes){

	    auto_regressive_add <- auto_regressive[i,][-i]
	    auto_regressive_generic <- c(auto_regressive_generic, auto_regressive_add)

	  }

	  auto_regressive_matrix <-
	    matrix(c(auto_regressive_user, auto_regressive_generic),
	           nrow = internal_list$info_model$n_processes,
	           ncol = internal_list$info_model$n_processes,
	           byrow = TRUE)

	  rownames(auto_regressive_matrix) <- c("user_names","generic_names")

	  internal_list$info_variables$names_time_invariant_unobserved_autoregressive <-
	    auto_regressive_matrix

	}

	#------------------------------------------
	# cross-lagged random coefficients
	#------------------------------------------

	if("cross-lagged" %in% heterogeneity){

	  ## time-invariant variables user names

	  cross_lagged <-
	    matrix(ncol = internal_list$info_model$n_processes,
	           nrow = internal_list$info_model$n_processes)

	  rownames(cross_lagged) <- colnames(cross_lagged) <-
	    user_names_processes

	  cross_lagged[,] <-
	    paste0("eta",
	            apply(expand.grid(rownames(cross_lagged), colnames(cross_lagged)),
	                 1,
	                 paste,
	                 collapse = ""))

	  cross_lagged_user <- character(0)

	  for (i in 1:internal_list$info_model$n_processes){

	    cross_lagged_add <- cross_lagged[i,][-i]
	    cross_lagged_user <- c(cross_lagged_user, cross_lagged_add)

	  }

	  ## time-invariant variables generic names
	  cross_lagged <-
	    matrix(ncol = internal_list$info_model$n_processes,
	           nrow = internal_list$info_model$n_processes)

	  rownames(cross_lagged) <- colnames(cross_lagged) <-
	    internal_list$info_variables$names_processes["generic_names", ]

	  cross_lagged[,] <-
	    paste0("eta",
	           apply(expand.grid(rownames(cross_lagged), colnames(cross_lagged)),
	                 1,
	                 paste,
	                 collapse = ""))

	  cross_lagged_generic <- character(0)

	  for (i in 1:internal_list$info_model$n_processes){

	    cross_lagged_add <- cross_lagged[i,][-i]
	    cross_lagged_generic <- c(cross_lagged_generic, cross_lagged_add)

	  }

	  cross_lagged_matrix <- matrix(c(cross_lagged_user, cross_lagged_generic),
	                                nrow = internal_list$info_model$n_processes,
	                                ncol = internal_list$info_model$n_processes,
	                                byrow = TRUE)

	  rownames(cross_lagged_matrix) <- c("user_names","generic_names")

	  internal_list$info_variables$names_time_invariant_unobserved_cross_lagged <-
	    cross_lagged_matrix

	}

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}

## test/development


