#' create_long_data
#'
#' changes data set to be in long format, including lagged effects for
#' all time_varying_variables.
#' @param data data set
#' @param time_varying_variables List of character vectors containing names of the time-varying
#'  variables. The number entries in the list corresponds to the number of univariate time-series.
#'  Each character vector contains the time-ordered variable names of a univariate time-series
#'  starting with the first measurement occasion.
#' @param time_invariant_variables List of character vectors containing names of the time-invariant
#'  variables. List must have the same length as list in argument \code{time_varying_variables}.
#'  @keywords internal
create_long_data <- function(data,
                             time_varying_variables,
                             time_invariant_variables){

  data$id <- 1:nrow(data)

  observed_variables <- unique(c(unlist(time_varying_variables),
                                 unlist(time_invariant_variables)))

  temporary_names <- vector("list", length(time_varying_variables))
  for(process in 1:length(time_varying_variables)){
    temporary_names[[process]] <- paste0(LETTERS[process], "_", 1:length(time_varying_variables[[process]]))
  }

  temporary_data <- data[,c("id", observed_variables)]
  current_names <- colnames(temporary_data)
  for(i in 1:length(time_varying_variables)){
    for(j in 1:length(time_varying_variables[[i]])){
      current_names[current_names == time_varying_variables[[i]][[j]]] <- temporary_names[[i]][[j]]
    }
  }
  colnames(temporary_data) <- current_names

  long_data <- temporary_data %>%
    pivot_longer(
      unlist(temporary_names),
      cols_vary = "slowest",
      names_to = c(".value", "time"),
      names_pattern = "(.)_(.)"
    )

  long_data$time <- as.numeric(long_data$time)

  for(process in 1:length(time_varying_variables)){
    long_data[[paste0(LETTERS[process], "_previous")]] <- NA

    for(i in unique(long_data$time)){
      if(i == 1)
        next
      long_data[[paste0(LETTERS[process], "_previous")]][long_data$time == i] <-
        long_data[[LETTERS[process]]][long_data$time == i-1]
    }
  }

  return(long_data)
}

