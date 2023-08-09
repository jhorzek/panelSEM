## Changelog:
# CG 0.0.4 2023-08-07: changed part on the linear additive model in the
#                      fill_in_model_specification_open_mx function
# CG 0.0.3 2023-05-24: changed preamble to be consistent with other functions
# 					   replaced arguments homogeneity and additive by heterogeneity
# CG 0.0.2 2023-04-18: insert console output for debugging
# JO 0.0.1 2023-04-18: initial programming

## Documentation
#' @title Specify the Model Syntax
#' @description Creates a \code{lavaan} or \code{OpenMx} model based in the user specified arguments in the \code{fit_panel_sem} function.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted internal_list with slot \code{internal_list$model_syntax} filled in.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598

fill_in_model_specification <- function(internal_list){

  # function name
  fun.name <- "fill_in_model_specification"

  # function version
  fun.version <- "0.0.3 2023-05-24"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )


  if(internal_list$info_model$use_open_mx)
    return(fill_in_model_specification_open_mx(internal_list))

  return(fill_in_model_specification_lavaan(internal_list))
}

#' fill_in_model_specification_open_mx
#'
#' Creates an OpenMx model based in the user specification in internal_list
#' @param internal_list internal list object
#' @return internal_list, where OpenMx model is added to internal_list$model_syntax
fill_in_model_specification_open_mx <- function(internal_list){

  # function details for debugging
  fun.name <- "fill_in_model_specification"
  fun.version <- "0_0_2 2023_04_18"
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  parameter_table <- internal_list$info_parameters$parameter_table

  variables <- get_variables(parameter_table)

  # add parameters from parameter table
  mx_model <- OpenMx::mxModel(type = "RAM",
                              manifestVars = variables$manifests,
                              latentVars = variables$latents,
                              OpenMx::mxData(observed = internal_list$info_data$data,
                                             type = "raw"),
                              OpenMx::mxPath(from = "one",
                                             to = variables$manifests,
                                             free = TRUE))
  warning("Freely estimating all intercepts of observed variables. Is this correct?")

  for(i in 1:nrow(parameter_table)){

    is_algebra <- parameter_table$algebra[i] != ""
    mx_model <- OpenMx::mxModel(mx_model,
                                OpenMx::mxPath(
                                  from = parameter_table$outgoing[i],
                                  to = parameter_table$incoming[i],
                                  values = parameter_table$value[i],
                                  labels =  ifelse(parameter_table$label[i] == "", NA,
                                                   ifelse(is_algebra,
                                                          paste0(parameter_table$label[i], "[1,1]"),
                                                          parameter_table$label[i])),
                                  free =  parameter_table$free[i],
                                  arrows =  ifelse(parameter_table$op[i] %in% c("=~", "~"), 1, 2)
                                ))
  }

  # check for algebras
  if(internal_list$info_parameters$has_algebras){
    # we have to check if all of the parameters used in the algebras
    # are already in the model:
    algebra_parameters <- c()
    for(i in which(parameter_table$algebra != "")){

      target <- parameter_table$label[i]
      algebra <- parameter_table$algebra[i]
      mx_algebra <- OpenMx::mxAlgebraFromString(algebra, name = target)

      # add algebra to model
      mx_model <- OpenMx::mxModel(mx_model,
                                  mx_algebra
      )

      # check the names of the parameters used in the algebra
      algebra_elements <- extract_algebra_elements(mxAlgebra_formula = mx_algebra$formula)
      # remove definition variables
      algebra_parameters <- c(algebra_parameters,
                              algebra_elements[!grepl(pattern = "^data.", x = algebra_elements)])
    }

    algebra_parameters <- unique(algebra_parameters)

    model_parameters <- OpenMx::omxGetParameters(mx_model)

    add_parameters <- algebra_parameters[!algebra_parameters %in% names(model_parameters)]


    mx_model <- OpenMx::mxModel(mx_model,
                                mxMatrix(name = "algebra_parameters",
                                         values = 0,
                                         free = TRUE,
                                         nrow = 1,
                                         ncol = length(add_parameters),
                                         labels = add_parameters))
  }

  internal_list$model_syntax$OpenMx <- mx_model

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )
  # return internal list
  return(internal_list)

}

#' fill_in_model_specification_lavaan
#'
#' Creates a lavaan model based in the user specification in internal_list
#' @param internal_list internal list object
#' @return internal_list, where lavaan is added to internal_list$model_syntax
fill_in_model_specification_lavaan <- function(internal_list){

  # function details for debugging
  fun.name <- "fill_in_model_specification"
  fun.version <- "0_0_2 2023_04_18"
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  if(internal_list$info_parameters$has_algebras)
    stop("lavaan does not allow for algebras. Try use_open_mx = TRUE")

  parameter_table <- internal_list$info_parameters$parameter_table

  if(any(grepl(pattern = "^data.", x = parameter_table$label)))
    stop("lavaan does not allow for definition variables")

  # add parameters from parameter table
  model_syntax <- c()

  for(i in 1:nrow(parameter_table)){

    if(parameter_table$op[i] %in% c("~", "~~")){
      model_syntax <- c(model_syntax,
                        paste0(parameter_table$incoming[i], " ",
                               parameter_table$op[i], " ",
                               ifelse(!parameter_table$free[i],
                                      paste0(parameter_table$value[i], "*"),
                                      paste0(parameter_table$label[i], "*")),
                               parameter_table$outgoing[i])
      )
    }else if(parameter_table$op[i] == "=~"){
      model_syntax <- c(model_syntax,
                        paste0(parameter_table$outgoing[i], " ",
                               parameter_table$op[i], " ",
                               ifelse(!parameter_table$free[i],
                                      paste0(parameter_table$value[i], "*"),
                                      paste0(parameter_table$label[i], "*")),
                               parameter_table$incoming[i])
      )
    }else{
      stop("Unknown operator ", parameter_table$op[i], ".")
    }
  }

  model_syntax <- paste0(
    c(paste0("# panelSEM\n#-- Syntax generated with function version ", fun.version, " --"),
      model_syntax),
    collapse = "\n"
  )

  internal_list$model_syntax$lavaan <- model_syntax

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )
  # return internal list
  return(internal_list)
}
