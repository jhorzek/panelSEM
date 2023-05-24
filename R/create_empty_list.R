## Changelog:
# CG 0.0.2 2023-05-24: replaced arguments homogeneity and additive by heterogeneity
# CG 0.0.1 2023-02-21: initial programming



## Documentation
#' @title Create Empty Internal List
#' @description Creates an empty internal list of predefined structure. The
#' list will subsequently be filled while the function
#' \code{\link{fit_panel_sem}} is running.
#' @param verbose Integer number describing verbosity of console output.
#' 0...no output (default); 1...user messages; 2...debugging-relevant messages.
#' @return An empty internal list with the following structure:\cr
#'  \tabular{lll}{
#'     List of XX\cr
#'    \code{$info_data}: List of XX       \tab \tab  \cr
#'      \code{..$data}: \code{data.frame}    \tab \tab Data set as \code{data.frame}.\cr
#'      \code{..$n_obs}: \code{int(0)}   \tab \tab Number of observations.\cr
#'      \code{..$n_var}: \code{int(0)}   \tab \tab Total number of variables in the data set.\cr
#'      \code{..$var_names}: \code{chr(0)}  \tab \tab Names of variables.\cr
#'    \code{$info_model}: List of XX        \tab \tab \cr
#'      \code{..$n_ocassions}: \code{int(0)}  \tab \tab Number of measurement occasions. \cr
#'      \code{..$n_processes}: \code{int(0)}  \tab \tab Number of dynamic processes. \cr
#'      \code{..$n_time_invariant} \code{int(0)}  \tab \tab  Number of observed time-invariant variables \cr
#'      \code{..$linear} \code{logical(0)}  \tab \tab \code{TRUE} if the model is linear in observed variables.\cr
#'      \code{..$heterogeneity} \code{logical(0)}  \tab \tab Specify type of observed heterogeneity.\cr
#'      \code{..$use_open_mx} \code{logical(0)}  \tab \tab If \code{TRUE}, \code{OpenMx} is used, otherwise \code{lavaan}.\cr
#'    \code{$info_variables}: List of XX        \tab \tab \cr
#'      \code{..$user_names_time_varying}: \code{char[0,0]}   \tab \tab User-Specified names of observed time-varying variables. \cr
#'      \code{..$generic_names_time_varying}: \code{char[0,0]}   \tab \tab Generic names of observed time-varying variables. \cr
#'      \code{..$names_processes}: \code{char[0,0]}   \tab \tab Names of dynamic processes. \cr
#'      \code{..$info_time_invariant}: \code{char[0,0]}   \tab \tab Names and causal structure of observed time-invariant variables. \cr
#'      \code{..$names_time_invariant_unique}: \code{char[0,0]}   \tab \tab Names of unique observed time-invariant variables. \cr
#'      \code{..$names_time_invariant_unobserved}: \code{char[0,0]}   \tab \tab Names of UNobserved time-invariant variables. \cr
#'    \code{$info_parameters}: List of XX \tab \tab \cr
#'      \code{..$parameter_list}: \code{data.frame} \tab \tab Table with labelled model parameters.\cr
#'    \code{model_matrices}: List of XX \tab \tab \cr
#'     \code{..$C_labels}: \code{char[0,0]}  \tab \tab Character matrix of labels of structural coefficients. \cr
#'     \code{..$Psi_labels}: \code{char[0,0]}  \tab \tab Character matrix of labels of covariance matrix. \cr
#'    \code{$model_syntax}: List of XX \tab \tab \cr
#'      \code{..$lavaan}: \code{list} \tab \tab Model specified in \code{lavaan}. \cr
#'      \code{..$OpenMx}: \code{list} \tab \tab Model specified in \code{OpenMx}. \cr
#'    \code{$control}: List of XX     \tab \tab \cr
#'      \code{..$verbose}: \code{int(0)}     \tab \tab Verbosity of console output.\cr
#'    \code{$tables}: List of XX      \tab \tab \cr
#'      \code{..$summary}: \code{data.frame}  \tab \tab Information for \code{\link{summary.panelSEM}} function.\cr
#'      \code{..$print}: \code{data.frame}  \tab \tab Information for \code{\link{print.panelSEM}}  function.\cr
#'      \code{..$coef}: \code{data.frame}  \tab \tab Information for \code{\link{coef.panelSEM}}  function.
#'  }
#'
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598


## Function definition
create_empty_list <- function( verbose = NULL ){

	# function name
	fun.name <- "create_empty_list"

	# function version
	fun.version <- "0.0.12 2023-02-21"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# check verbose argument
	verbose <- handle_verbose_argument( verbose )

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# internal list

	internal_list <- list(

		# data
	  info_data = list(

	    # data set
	    # data.frame object
	    "data" = data.frame(0),

	    # number of observations
	    # Integer number
	    "n_obs" = integer(0),

	    # total number of observed variables in data set
	    # Integer number
	    "n_var" = integer(0),

	    # names of observed variables in data set
	    # character vector
	    "var_names" = character(0)

	  ), # end of data list

		# model info
		info_model = list(

			# number of measurement occasions
			# a single number, normally an integer
			"n_occasions" = as.integer(0),

			# number of dynamic processes
			# a single number, normally an integer
			"n_processes" = as.integer(0),

			# number of observed time-invariant variables
			# a single number, normally an integer
			"n_time_invariant" = as.integer(0),

			# Is the model linear in observed variables?
			# If yes, linear == TRUE.
			# a single logical value
			"linear" = logical(0),

			# Type of unobserved heterogeneity.
			# a character vector
			"heterogeneity" = character(0),

			# Is OpenMx used?
			# If yes, use_open_mx == TRUE. If FALSE, lavaan is used.
			# a single logical value
			"use_open_mx" = logical(0)

		), # end info_model_list

		info_variables = list(

		  # user-specified names of observed time-varying variables
		  # character matrix
		  "user_names_time_varying" = character(0),

		  # generic names of observed time-varying variables
		  # character matrix
		  "generic_names_time_varying" = character(0),

		  # names of dynamic processes
		  # character matrix
		  "names_processes" = character(0),

		  # names and causal structure of observed time-invariant variables
		  # character matrix
		  "info_time_invariant_variables" = character(0),

		  # names of observed time-invariant variables
		  # character matrix
		  "names_time_invariant_unique" = character(0),

		  # names of unobserved time-invariant variables
		  # character matrix
		  "names_time_invariant_unobserved" = character(0)

		), # end info_variables list

		info_parameters = list(

		    # list of structural coefficients contains labels, starting values and
		    # several other information
		    "C_table" = data.frame(0),

		    # list of covariance parameters contains labels, starting values and
		    # several other information
		    "Psi_table" = data.frame(0)

		), # end info_parameter list

		model_matrices = list(

		  # matrix of labels of structural coefficients

		  # matrix of structural coefficients
		  "C_labels" = character(0),

		  # covariance matrix
		  "Psi_labels" = character(0),

		  # filter matrix / selection matrix for selecting observed variables only
		  "select_observed_only" = numeric(0)

		), # end model_matrices list

		model_syntax = list(

		  # model syntax for lavaan
		  # list of characters

		  "lavaan" = character(0),

		  # model syntax for lavaan
		  # list of characters

		  "OpenMx" = character(0)

		), # end of model_syntax list

		# control list
		control = list(

		# verbosity of console output
		# a number, 0...no output (default), 1...user messages,
		# 2...debugging-relevant messages
		"verbose" = verbose

		) # end of control list

	) # end of internal list

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}

## test/development

