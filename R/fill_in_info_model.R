## Changelog:
# CG 0.0.1 2023-02-20: initial programming

## Documentation
#' @title Get Model Information
#' @description Provide information about the dynamic panel data model.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @param time_varying_variables List of character vectors containing names of the time-varying
#'  variables. Within each vector, the variable names must be time-ordered starting with the first
#'  measurement ocassion.
#' @param time_invariant_variables List of character vectors containing names of the time-invariant
#'  variables. List must have the same length as list in argument \code{time_varying_variables}.
#' @param n_occasions Integer number indicating the number of measurement occasions.
#' @param homogeneous Logical (TRUE / FALSE = default) indicating if the model contains unobserved heterogeneity (TRUE).
#' @param linear Logical (TRUE = default / FALSE) indicating if the model is linear in observed variables (TRUE).
#' @param additive Logical (TRUE = default / FALSE) indicating if the unobserved heterogeneity is additive (TRUE).
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


## Function definition
fill_in_info_model <- function(internal_list = NULL,
                               time_varying_variables = NULL,
                               time_invariant_variables = NULL,
                               homogeneous = FALSE,
                               linear = TRUE,
                               additive  = TRUE){

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

	# GENERAL MODEL INFORMATION
	## extract model information from the arguments
	internal_list$info_model$n_occasions <-
	  length(time_varying_variables[[1]])
	internal_list$info_model$n_processes <-
	  sum(lengths(time_varying_variables)) /
	  internal_list$info_model$n_occasions
	internal_list$info_model$n_time_invariant <-
	  length(unique(unlist(time_invariant_variables)))
	internal_list$info_model$additive <- additive
	internal_list$info_model$linear <- linear
	internal_list$info_model$homogeneous  <- homogeneous
	internal_list$info_model$use_open_mx <- use_open_mx


	# OBSERVED VARIABLES
	## time-varying variables
	### generic names time-varying variables

	table_generic_names_colnames <-
	  as.character(1:internal_list$info_model$n_occasions)
	table_generic_names_rownames <-
	  paste0("process ",LETTERS[1:internal_list$info_model$n_processes])

	table_generic_names <-
	  matrix(
	    paste0(rep(gsub("process ", "",table_generic_names_rownames),
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
	  rownames(internal_list$info_variables$user_names_time_varying)

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

	# UNOBSERVED VARIABLES
	## time-invariant variables
	# TODO: fill in the structural error terms if desired. These are
	# the error terms (epsilon-terms) of the time-varying and time-invariant
	# variables

	## time-invariant variables

	time_invariant_variables_unobserved_user_names <-
	paste0("eta", user_names_processes)

	time_invariant_variables_unobserved_generic_names <-
	paste0("eta",
	   rownames(internal_list$info_variables$generic_names_time_varying))

	table_user_names_processes <-
	  matrix(ncol = internal_list$info_model$n_processes,
	         nrow = 2)

	table_user_names_processes[1,] <-
	  time_invariant_variables_unobserved_user_names
	table_user_names_processes[2,] <-
	  time_invariant_variables_unobserved_generic_names

	rownames(table_user_names_processes) <- c("user_names","generic_names")

	internal_list$info_variables$names_time_invariant_unobserved <-
	  table_user_names_processes

	# PARAMETER LIST FOR STRUCTURAL COEFFICIENT
	## compute total number of structural coefficients in the model
	### eta-variables onto initial variables
	n_param_C_unique <- internal_list$info_model$n_processes *
	  internal_list$info_model$n_processes

	### z-variables onto initial variables
	n_param_C_unique <- n_param_C_unique +
	  internal_list$info_model$n_processes *
	  internal_list$info_model$n_time_invariant

	### eta variables onto time-varying NON-initial variables
	n_param_C_unique <- n_param_C_unique +
	  (internal_list$info_model$n_processes *
	  (internal_list$info_model$n_occasions - 1))

	### z variables onto time-varying NON-initial variables
	n_param_C_unique <- n_param_C_unique +
	  sum(lengths(internal_list$info_variables$info_time_invariant_variables)) *
	  (internal_list$info_model$n_occasions - 1)

	### time-varying variables onto time-varying variables
	n_param_C_unique <- n_param_C_unique +
	  (internal_list$info_model$n_occasions-1) *
	  internal_list$info_model$n_processes^2

	## create parameter table of structural coefficients
	param_list_C_unique <- matrix(nrow = n_param_C_unique,
	                              ncol = 5)
	colnames(param_list_C_unique) <- c("incoming",
	                                   "outgoing",
	                                   "label",
	                                   "constrain",
	                                   "value")

	param_list_C_unique <- as.data.frame(param_list_C_unique)

	### eta-variables onto initial variables
	#### number of edges
	start_fill <- min(which(is.na(param_list_C_unique[,"incoming"])))
	number_entries <- internal_list$info_model$n_processes *
	  internal_list$info_model$n_processes
	end_fill <- start_fill + number_entries - 1

	#### name incoming variable
	param_list_C_unique[start_fill:end_fill,"incoming"] <-
	  rep(internal_list$info_variables$user_names_time_varying[,1],
	      each = (internal_list$info_model$n_processes))

	#### name outgoing variable
	param_list_C_unique[start_fill:end_fill,"outgoing"] <-
	  rep(internal_list$info_variables$names_time_invariant_unobserved[
	    "user_names",], internal_list$info_model$n_processes)

	#### constrained
	param_list_C_unique[start_fill:end_fill,"constrain"] <- FALSE

	#### value
	param_list_C_unique[start_fill:end_fill,"value"] <- NA

	#### label
	param_list_C_unique[start_fill:end_fill, "label"] <-
	  paste0("c_",
	         param_list_C_unique[start_fill:end_fill,"incoming"],
	         "_",
	         param_list_C_unique[start_fill:end_fill,"outgoing"])

	### z-variables onto initial variables
	#### number of edges
	start_fill <- min(which(is.na(param_list_C_unique[,"incoming"])))
	number_entries <- internal_list$info_model$n_processes *
	  internal_list$info_model$n_time_invariant
	end_fill <- start_fill + number_entries - 1

	#### name incoming variable
	param_list_C_unique[start_fill:end_fill,"incoming"] <-
	  rep(internal_list$info_variables$user_names_time_varying[,1],
	      each = (internal_list$info_model$n_time_invariant))

	#### name outgoing variable
	param_list_C_unique[start_fill:end_fill,"outgoing"] <-
	  rep(internal_list$info_variables$names_time_invariant_unique["user_names",],
	      internal_list$info_model$n_processes)

	#### constrained
	param_list_C_unique[start_fill:end_fill,"constrain"] <- FALSE

	#### value
	param_list_C_unique[start_fill:end_fill,"value"] <- NA

	#### label
	param_list_C_unique[start_fill:end_fill, "label"] <-
	  paste0("c_",
	         param_list_C_unique[start_fill:end_fill,"incoming"],
	         "_",
	         param_list_C_unique[start_fill:end_fill,"outgoing"])

	### eta variables onto NON-initial time-varying variables
	#### number of edges
	start_fill <- min(which(is.na(param_list_C_unique[,"incoming"])))
	number_entries <- internal_list$info_model$n_processes *
	  (internal_list$info_model$n_occasions - 1)
	end_fill <- start_fill + number_entries - 1

	#### name incoming variable
	param_list_C_unique[start_fill:end_fill,"incoming"] <-
	  as.vector(
	    t(
	      internal_list$info_variables$user_names_time_varying[
	        , (2:internal_list$info_model$n_occasions)]))

	#### name outgoing variable
	param_list_C_unique[start_fill:end_fill,"outgoing"] <-
	  rep(internal_list$info_variables$names_time_invariant_unobserved[
	    "user_names",], each = (internal_list$info_model$n_occasions - 1))

	#### constrained
	param_list_C_unique[start_fill:end_fill,"constrain"] <- TRUE

	#### value
	param_list_C_unique[start_fill:end_fill,"value"] <- 1

	#### label
	param_list_C_unique[start_fill:end_fill, "label"] <-
	  paste0("c_",
	         param_list_C_unique[start_fill:end_fill,"incoming"],
	         "_",
	         param_list_C_unique[start_fill:end_fill,"outgoing"])

	### z variables onto NON-initial time-varying variables
  #### number of edges

	start_fill <- min(which(is.na(param_list_C_unique[,"incoming"])))
	number_entries <-
	  sum(lengths(internal_list$info_variables$info_time_invariant_variables)) *
	  (internal_list$info_model$n_occasions - 1)
	end_fill <- start_fill + number_entries - 1

  #### name incoming variable
	name_incoming_variables <-
	  vector(mode = "list",
	         length = internal_list$info_model$n_processes)

	for (i in 1:internal_list$info_model$n_processes) {
	  name_incoming_variables[[i]] <-
	rep(internal_list$info_variables$user_names_time_varying[
	  i , (2:internal_list$info_model$n_occasions)],
	  each = lengths(
	    internal_list$info_variables$info_time_invariant_variables)[i])
	  	}

	param_list_C_unique[start_fill:end_fill,"incoming"] <-
	  as.vector(unlist(name_incoming_variables))

  #### name outgoing variable

	name_outgoing_variables <-
	  vector(mode = "list",
	         length = internal_list$info_model$n_processes)

	for (i in 1:internal_list$info_model$n_processes) {
	  name_outgoing_variables[[i]] <-
rep(as.vector(internal_list$info_variables$info_time_invariant_variables[[i]]),
    (internal_list$info_model$n_occasions - 1))
	}

	param_list_C_unique[start_fill:end_fill,"outgoing"] <-
	  as.vector(unlist(name_outgoing_variables))

	#### constrained
	param_list_C_unique[start_fill:end_fill,"constrain"] <- TRUE

	#### value

	labels_constrained <-
	  vector(mode = "list",
	         length = internal_list$info_model$n_processes)

	for (i in 1:internal_list$info_model$n_processes){
	  labels_constrained[[i]] <-
	paste0("c",
	       "_",
	       internal_list$info_variables$names_processes["user_names",i],
	       "_",
	       internal_list$info_variables$info_time_invariant_variables[[i]])

	}



	param_list_C_unique[start_fill:end_fill,"value"] <-
	  unlist(
	    rep(
	      labels_constrained,
	      each = (internal_list$info_model$n_occasions - 1)))

	#### label
	param_list_C_unique[start_fill:end_fill, "label"] <-
	  paste0("c_",
	         param_list_C_unique[start_fill:end_fill,"incoming"],
	         "_",
	         param_list_C_unique[start_fill:end_fill,"outgoing"])


	### time-varying variables onto time-varying variables
  #### number of edges

	start_fill <- min(which(is.na(param_list_C_unique[,"incoming"])))
	number_entries <-
	  (internal_list$info_model$n_occasions-1) *
	  internal_list$info_model$n_processes^2
	end_fill <- start_fill + number_entries - 1

  #### name incoming variable

	name_incoming_variables <-
	  vector(mode = "list",
	         length = (internal_list$info_model$n_occasions - 1))

	name_incoming_variables_step <-
	  vector(mode = "list",
	         length = internal_list$info_model$n_processes)

	for (t in 2:internal_list$info_model$n_occasions){
	for (i in 1:internal_list$info_model$n_processes){

	  name_incoming_variables_step[[i]] <-
	  rep(internal_list$info_variables$user_names_time_varying[i, t],
	    internal_list$info_model$n_processes)

	}
	  name_incoming_variables[[t-1]] <- name_incoming_variables_step
	}

	param_list_C_unique[start_fill:end_fill,"incoming"] <-
	  as.vector(unlist(name_incoming_variables))

  #### name outgoing variable

	name_outgoing_variables <-
	  vector(mode = "list",
	         length = (internal_list$info_model$n_occasions - 1))

	for (t in 1:(internal_list$info_model$n_occasions - 1)){

	    name_outgoing_variables[[t]] <-
	      rep(internal_list$info_variables$user_names_time_varying[ , t],
	          internal_list$info_model$n_processes)

	    }

	param_list_C_unique[start_fill:end_fill,"outgoing"] <-
	  as.vector(unlist(name_outgoing_variables))

	#### constrained
	param_list_C_unique[start_fill:end_fill,"constrain"] <- TRUE

	#### value

	labels_constrained <-
	  vector(mode = "list",
	         length = internal_list$info_model$n_processes)

	for (i in 1:internal_list$info_model$n_processes){
	  labels_constrained[[i]] <-
	    paste0("c",
	           "_",
	           internal_list$info_variables$names_processes["user_names",i],
	           "_",
	           internal_list$info_variables$names_processes["user_names",])

	}

	param_list_C_unique[start_fill:end_fill,"value"] <-
	  unlist(
	    rep(
	      labels_constrained,
	      (internal_list$info_model$n_occasions - 1)))

	#### label
	param_list_C_unique[start_fill:end_fill, "label"] <-
	  paste0("c_",
	         param_list_C_unique[start_fill:end_fill,"incoming"],
	         "_",
	         param_list_C_unique[start_fill:end_fill,"outgoing"])

	# fill into internal_list

	internal_list$info_parameters$paramater_list$C_table <- param_list_C_unique


	# COVARIANCE MATRIX PSI
	## compute total number of variables in the model

	n_total <- internal_list$info_model$n_occasions *
	  internal_list$info_model$n_processes +
	  internal_list$info_model$n_time_invariant +
	  internal_list$info_model$n_processes

	names_variables <-
	  c(internal_list$info_variables$names_time_invariant_unobserved["user_names",],
	    internal_list$info_variables$names_time_invariant["user_names",],
	    c(internal_list$info_variables$user_names_time_varying))

	Psi <- matrix(ncol = n_total,
	              nrow = n_total)

	rownames(Psi) <- colnames(Psi) <- names_variables

	### (co-)variances among eta-variables
	Psi_eta <- matrix(ncol = internal_list$info_model$n_processes,
	                  nrow = internal_list$info_model$n_processes)

	rownames(Psi_eta) <- colnames(Psi_eta) <-
	  internal_list$info_variables$names_time_invariant_unobserved["user_names",]

	Psi_eta[,] <-
	  paste0("psi",
	         "_",
	         apply(expand.grid(rownames(Psi_eta), colnames(Psi_eta)),
	               1,
	               paste,
	               collapse = "_"))

	Psi_eta[lower.tri(Psi_eta)] <- Psi_eta[upper.tri(Psi_eta)]

	Psi[1:internal_list$info_model$n_processes,
	    1:internal_list$info_model$n_processes] <- Psi_eta


	### (co-)variances among z-variables
	Psi_z <-
	  matrix(ncol = internal_list$info_model$n_time_invariant ,
	         nrow = internal_list$info_model$n_time_invariant)

	rownames(Psi_z) <- colnames(Psi_z) <-
	  internal_list$info_variables$names_time_invariant["user_names",]

	Psi_z[,] <-
	  paste0("psi",
	         "_",
	         apply(expand.grid(rownames(Psi_z), colnames(Psi_z)),
	               1,
	               paste,
	               collapse = "_"))

	Psi_z[lower.tri(Psi_z)] <- Psi_z[upper.tri(Psi_z)]

	start_fill <- min(which(is.na(diag(Psi))))
	end_fill <- start_fill + internal_list$info_model$n_time_invariant -1
	Psi[start_fill:end_fill,
	    start_fill:end_fill] <- Psi_z

	### (co-)variances among initial variables
	Psi_init <- matrix(ncol = internal_list$info_model$n_processes,
	                   nrow = internal_list$info_model$n_processes)

	rownames(Psi_init) <- colnames(Psi_init) <-
	  as.character(internal_list$info_variables$user_names_time_varying[,1])

	Psi_init[,] <-
	  paste0("psi",
	         "_",
	         apply(expand.grid(rownames(Psi_init), colnames(Psi_init)),
	               1,
	               paste,
	               collapse = "_"))

	Psi_init[lower.tri(Psi_init)] <- Psi_init[upper.tri(Psi_init)]

	start_fill <- min(which(is.na(diag(Psi))))
	end_fill <- start_fill + internal_list$info_model$n_processes -1
	Psi[start_fill:end_fill,
	    start_fill:end_fill] <- Psi_init

	### (co-)variances among NON-initial time-varying variables
	Psi_time_varying <-
	  matrix(ncol = (internal_list$info_model$n_processes *
	                   (internal_list$info_model$n_occasions - 1)),
	         nrow = (internal_list$info_model$n_processes *
	                   (internal_list$info_model$n_occasions - 1)))

	rownames(Psi_time_varying) <- colnames(Psi_time_varying) <-
	  as.character(
	    internal_list$info_variables$user_names_time_varying[,
	                                                         2:internal_list$info_model$n_occasions])

	labels_psi_time_varying <-
	  vector(mode = "list",
	         length = (internal_list$info_model$n_processes))

	for (i in 1:internal_list$info_model$n_processes){
	  labels_psi_time_varying[[i]] <-
	    paste0("psi",
	           "_",
	           internal_list$info_variables$names_processes["user_names",i],
	           "_",
	           internal_list$info_variables$names_processes["user_names",i])

	}

	Psi_time_varying[,] <- 0
	diag(Psi_time_varying) <-
	  unlist(
	    rep(
	      labels_psi_time_varying,
	      (internal_list$info_model$n_occasions - 1)))

	start_fill <- min(which(is.na(diag(Psi))))
	end_fill <- start_fill + (internal_list$info_model$n_processes *
	                            (internal_list$info_model$n_occasions - 1)) -1
	Psi[start_fill:end_fill,
	    start_fill:end_fill] <- Psi_time_varying


	## fill in Covariance Matrix

	internal_list$model_matrices$Psi_labels <- Psi

	# PARAMETER TABLE FOR COVARIANCE MATRIX
	## create parameter table of covariance parameters

	Psi_vector <- Psi[t(upper.tri(Psi, diag = TRUE))]

	Psi_vector <- Psi_vector[!is.na(Psi_vector)]

	Psi_vector <- Psi_vector[Psi_vector != 0]

	n_param_Psi = length(Psi_vector)

	param_list_Psi <- matrix(nrow = n_param_Psi,
	                         ncol = 5)

	colnames(param_list_Psi) <- c("incoming",
	                              "outgoing",
	                              "label",
	                              "constrain",
	                              "value")

	param_list_Psi <- as.data.frame(param_list_Psi)

	start_time_varying <- nrow(param_list_Psi) -
	  ((internal_list$info_model$n_occasions - 1) *
	     internal_list$info_model$n_processes) + 1
	end_time_varying <- nrow(param_list_Psi)

	## fill in column value

	param_list_Psi$value[start_time_varying:end_time_varying] <-
	  Psi_vector[start_time_varying:end_time_varying]

	## fill in column constrain

	param_list_Psi$constrain <- FALSE
	param_list_Psi$constrain[start_time_varying:end_time_varying] <- TRUE


	## fill in column label

	Psi_vector[start_time_varying:end_time_varying] <-
	  paste0("psi_",
	         as.vector(
	           internal_list$info_variables$user_names_time_varying[
	             ,
	             2:internal_list$info_model$n_occasions]),
	         "_",
	         as.vector(
	           internal_list$info_variables$user_names_time_varying[
	             ,
	             2:internal_list$info_model$n_occasions]))


	param_list_Psi$label <- Psi_vector

	## fill in column incoming and outgoing

	### (co-)variances among eta-variables

	start_fill <- 1
	end_fill <- ncol(Psi_eta) * (ncol(Psi_eta) + 1) / 2

	Psi_incoming <- numeric(0)
	Psi_outgoing <- numeric(0)

	for (i in 1:ncol(Psi_eta)){
	  Psi_incoming <- c(Psi_incoming,
	                    rep(rownames(Psi_eta)[i],(ncol(Psi_eta)+1-i)))
	}

	for (i in 1:ncol(Psi_eta)){
	  for (j in i:ncol(Psi_eta)){
	    Psi_outgoing <- c(Psi_outgoing,rownames(Psi_eta)[j])
	  }
	}

	param_list_Psi$incoming[start_fill:end_fill] <- Psi_incoming
	param_list_Psi$outgoing[start_fill:end_fill] <- Psi_outgoing

	### (co-)variances among z-variables

	start_fill <- min(which(is.na(param_list_Psi[,"incoming"])))
	number_entries <- ncol(Psi_z) * (ncol(Psi_z) + 1) / 2
	end_fill <- start_fill + number_entries -1

	Psi_incoming <- numeric(0)
	Psi_outgoing <- numeric(0)

	for (i in 1:ncol(Psi_z)){
	  Psi_incoming <- c(Psi_incoming, (rep(rownames(Psi_z)[i],(ncol(Psi_z)+1-i))))
	}

	for (i in 1:ncol(Psi_z)){
	  for (j in i:ncol(Psi_z)){
	    Psi_outgoing <- c(Psi_outgoing,rownames(Psi_z)[j])
	  }
	}

	param_list_Psi$incoming[start_fill:end_fill] <- Psi_incoming
	param_list_Psi$outgoing[start_fill:end_fill] <- Psi_outgoing

	### (co-)variances among initial variables

	start_fill <- min(which(is.na(param_list_Psi[,"incoming"])))
	number_entries <- ncol(Psi_init) * (ncol(Psi_init) + 1) / 2
	end_fill <- start_fill + number_entries -1

	Psi_incoming <- numeric(0)
	Psi_outgoing <- numeric(0)

	for (i in 1:ncol(Psi_init)){
	  Psi_incoming <- c(Psi_incoming,
	                    rep(rownames(Psi_init)[i],(ncol(Psi_init)+1-i)))
	}

	for (i in 1:ncol(Psi_init)){
	  for (j in i:ncol(Psi_init)){
	    Psi_outgoing <- c(Psi_outgoing,rownames(Psi_init)[j])
	  }
	}

	param_list_Psi$incoming[start_fill:end_fill] <- Psi_incoming
	param_list_Psi$outgoing[start_fill:end_fill] <- Psi_outgoing

	### (co-)variances among NON-initial time-varying variables

	start_fill <- min(which(is.na(param_list_Psi[,"incoming"])))
	number_entries <- internal_list$info_model$n_processes *
	  (internal_list$info_model$n_occasions - 1)
	end_fill <- start_fill + number_entries -1

	param_list_Psi$incoming[start_fill:end_fill] <-
	  param_list_Psi$outgoing[start_fill:end_fill] <-
	  as.vector(
	    internal_list$info_variables$user_names_time_varying[
	      ,
	      2:internal_list$info_model$n_occasions])

	# fill into internal list

	internal_list$info_parameters$paramater_list$Psi_table <- param_list_Psi



	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}

## test/development


