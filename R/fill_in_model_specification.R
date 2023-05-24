## Changelog:
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
  fun.version <- "0.0.2 2023-04-18"

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

  #note: OpenMx does not allow for the format used
  # in the other functions

  # extract directed and undirected effects:

  directed <- internal_list$info_parameters$C_table
  undirected <- internal_list$info_parameters$Psi_table

  # separate latent and manifest

  latents <- unique(c(directed$outgoing[!directed$outgoing %in% internal_list$info_data$var_names],
                      directed$incoming[!directed$incoming %in% internal_list$info_data$var_names]))
  manifests <- unique(c(directed$outgoing[directed$outgoing %in% internal_list$info_data$var_names],
                        directed$incoming[directed$incoming %in% internal_list$info_data$var_names]))

  # initialize model. We will fill in the elements below
  model <- OpenMx::mxModel(model = paste0("panelSEM specified with function version ", fun.version),
                           type = "RAM",
                           manifestVars = manifests,
                           latentVars = latents,
                           OpenMx::mxPath(from = 'one', to = manifests),
                           OpenMx::mxData(observed = internal_list$info_data$data,
                                          type = "raw"))

  # DIRECTED EFFECTS
  # The value field of directed mixes actual values and the
  # parameter labels. This is very useful for lavaan, but not for
  # OpenMx. We must therefore separate the two cases first:
  ## check if a numeric value is given:
  has_value <- grepl(pattern = "^[0-9.]+$", directed$value)
  values <- rep(NA, length(has_value))
  values[has_value] <- as.numeric(directed$value[has_value])

  has_label <- !grepl(pattern = "^[0-9.]+$", directed$value)
  label <- rep(NA, length(has_label))
  label[has_label] <- directed$value[has_label]

  model <- OpenMx::mxModel(model,
                           OpenMx::mxPath(from = directed$outgoing,
                                          to = directed$incoming,
                                          connect = "single",
                                          values = values,
                                          labels = label,
                                          arrows = 1)
  )

  # UNDIRECTED EFFECTS
  # The value field of undirected mixes actual values and the
  # parameter labels. This is very useful for lavaan, but not for
  # OpenMx. We must therefore separate the two cases first:
  ## check if a numeric value is given:
  has_value <- grepl(pattern = "^[0-9.]+$", undirected$value)
  values <- rep(NA, length(has_value))
  values[has_value] <- as.numeric(undirected$value[has_value])

  has_label <- !grepl(pattern = "^[0-9.]+$", undirected$value)
  label <- rep(NA, length(has_label))
  label[has_label] <- undirected$value[has_label]

  model <- OpenMx::mxModel(model,
                           OpenMx::mxPath(from = undirected$outgoing,
                                          to = undirected$incoming,
                                          connect = "single",
                                          values = values,
                                          labels = label,
                                          arrows = 2)
  )

  internal_list$model_syntax$OpenMx <- model

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

  # extract directed and undirected effects:

  directed <- internal_list$info_parameters$C_table
  latent_to_manifest <- (!directed$outgoing %in% internal_list$info_data$var_names) &
    (directed$incoming %in% internal_list$info_data$var_names)

  model_syntax <- c()

  if(length(directed$incoming[!latent_to_manifest]) > 0)
    model_syntax <- c(model_syntax,
                      paste0(directed$incoming[!latent_to_manifest],
                             " ~ ",
                             ifelse(is.na(directed$value[!latent_to_manifest]), "NA", directed$value[!latent_to_manifest]),
                             " * ",
                             directed$outgoing[!latent_to_manifest])
    )

  if(length(directed$outgoing[latent_to_manifest]) > 0)
    model_syntax <- c(model_syntax,
                      paste0(directed$outgoing[latent_to_manifest],
                             " =~ ",
                             ifelse(is.na(directed$value[latent_to_manifest]), "NA", directed$value[latent_to_manifest]),
                             " * ",
                             directed$incoming[latent_to_manifest])
    )

  undirected <- internal_list$info_parameters$Psi_table

  if(length(undirected$incoming) > 0)
    model_syntax <- c(model_syntax,
                      paste0(undirected$incoming, " ~~ ",
                             ifelse(is.na(undirected$value), "", paste0(undirected$value, " * ")),
                             undirected$outgoing)
    )

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
