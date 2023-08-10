#' check_panel_sem_specification
#'
#' checks if the user specified all arguments of fit_panel_sem correctly
#' @param specification list with user specified arguments
#' @return throws error in case of misspecification
#' @keywords internal
check_panel_sem_specification <- function(specification){

  with(data = specification,
       expr = {

         if((!is(data, "matrix")) && (!is(data, "data.frame")))
           stop("data must be a matrix or data.frame")

         if(!is(time_varying_variables, "list"))
           stop("time_varying_variables must be a list and not a ", class(time_varying_variables))

         # check if the number of occasions is equal for all processes:
         if(length(unique(lengths(time_varying_variables))) != 1)
           stop("The number of occasions in time_varying_variables must be the same for all processes.")

         if(!is(time_invariant_variables, "list"))
           stop("time_invariant_variables must be a list and not a ", class(time_invariant_variables))

         if(!is(linear, "logical"))
           stop("linear must be a logical and not a ", class(logical))


         if(length(heterogeneity) == 1){
           if(!heterogeneity %in% c("homogeneous", "additive", "cross-lagged"))
             stop("heterogeneity must be one of: ",
                  paste0(c("homogeneous", "additive", "cross-lagged", "c('additive', 'cross-lagged')"), collapse = ", "),
                  ".")
         }else if(length(heterogeneity) == 2){
           if(sort(heterogeneity) != c("additive", "cross-lagged"))
             stop("heterogeneity must be one of: ",
                  paste0(c("homogeneous", "additive", "cross-lagged", "c('additive', 'cross-lagged')"), collapse = ", "),
                  ".")
         }else{
           stop("heterogeneity must be one of: ",
                paste0(c("homogeneous", "additive", "cross-lagged", "c('additive', 'cross-lagged')"), collapse = ", "),
                ".")
         }

         if(!is(use_resamples, "logical"))
           stop("use_resamples must be a logical and not a ", class(use_resamples))

         if(!is(linear, "logical"))
           stop("linear must be a logical and not a ", class(linear))

         if(!is(verbose, "numeric"))
           stop("verbose must be an integer and not a ", class(verbose))

         # if(!all(sapply(dotdotdot, function(x) is(x,"NULL"))))
         #   stop("... is currently not supported and only implemented for future use cases.")

       })
}
