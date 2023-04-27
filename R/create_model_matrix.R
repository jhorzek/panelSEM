## Changelog:
# CG 0.0.1 2023-02-20: initial programming

## Documentation
#' @title Create Model Matrices
#' @description Create model matrices such as the matrix of structural
#' coefficients, the covariance matrix, and selection matrix.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @param matrix_type Character string describing the type of model matrix. Admissible values are: \code{"C"}, \code{"Psi"}, and \code{"selection"}.
#' @return A matrix of model parameters or a selection matrix.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598


## Function definition
create_model_matrix <- function(internal_list = NULL,
                                matrix_type = NULL){

  # function name
  fun.name <- "create_model_matrix"

  # function version
  fun.version <- "0.0.1 2023-04-25"

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

  homogeneous <- internal_list$info_model$homogeneous
  linear <- internal_list$info_model$linear
  additive <- internal_list$info_model$additive

  #############################################
  # COVARIANCE MATRIX
  #############################################

  #------------------------------------------
  # linear homogeneous model
  #------------------------------------------

  if(linear == TRUE &&
     homogeneous == TRUE &&
     matrix_type == "Psi"){

    n_total <- internal_list$info_model$n_occasions *
      internal_list$info_model$n_processes +
      internal_list$info_model$n_time_invariant

    names_variables <-
      c(internal_list$info_variables$names_time_invariant_unique["user_names",],
        c(internal_list$info_variables$user_names_time_varying))

    Psi <- matrix(ncol = n_total,
                  nrow = n_total)

    rownames(Psi) <- colnames(Psi) <- names_variables

    ### (co-)variances among z-variables
    Psi_z <-
      matrix(ncol = internal_list$info_model$n_time_invariant ,
             nrow = internal_list$info_model$n_time_invariant)

    rownames(Psi_z) <- colnames(Psi_z) <-
      internal_list$info_variables$names_time_invariant_unique["user_names",]

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


    #### contemporaneous (co-)variances
    Psi_time_varying_contemporaneous <-
      matrix(ncol = internal_list$info_model$n_processes,
             nrow = internal_list$info_model$n_processes)

    rownames(Psi_time_varying_contemporaneous) <-
      colnames(Psi_time_varying_contemporaneous) <-
      internal_list$info_variables$names_processes["user_names",]

    Psi_time_varying_contemporaneous[,] <-
      paste0("psi",
             "_",
             apply(expand.grid(rownames(Psi_time_varying_contemporaneous),
                               colnames(Psi_time_varying_contemporaneous)),
                   1,
                   paste,
                   collapse = "_"))

    Psi_time_varying_contemporaneous[lower.tri(Psi_time_varying_contemporaneous)] <-
      Psi_time_varying_contemporaneous[upper.tri(Psi_time_varying_contemporaneous)]

    for (i in 1:(internal_list$info_model$n_occasions - 1)){
    start_fill <- min(which(is.na(diag(Psi_time_varying))))
    end_fill <- start_fill + internal_list$info_model$n_processes -1
    Psi_time_varying[start_fill:end_fill,
                     start_fill:end_fill] <- Psi_time_varying_contemporaneous
    }

    #### serial covariances

    Psi_time_varying_serial <- character(0)

    for (i in 1:(length(diag(Psi_time_varying)) -
                 internal_list$info_model$n_processes)){
      Psi_time_varying_serial[i] <-
        paste0("psi",
               "_",
               colnames(Psi_time_varying)[i],
               "_",
               rownames(Psi_time_varying)[i+internal_list$info_model$n_processes])
    }

    Psi_time_varying[col(Psi_time_varying) - row(Psi_time_varying) ==
                       internal_list$info_model$n_processes] <-
      Psi_time_varying[col(Psi_time_varying) - row(Psi_time_varying) ==
                         -internal_list$info_model$n_processes] <-
      Psi_time_varying_serial


    #### fill into Psi matrix
    start_fill <- min(which(is.na(diag(Psi))))
    end_fill <- start_fill + (internal_list$info_model$n_processes *
                                (internal_list$info_model$n_occasions - 1)) -1
    Psi[start_fill:end_fill,
        start_fill:end_fill] <- Psi_time_varying

    ### serial covariances including initial variables

for (i in 1:internal_list$info_model$n_processes){
Psi[rownames(Psi) ==
as.character(internal_list$info_variables$user_names_time_varying[,1])[i],
colnames(Psi) ==
as.character(internal_list$info_variables$user_names_time_varying[,2])[i]] <-
Psi[rownames(Psi) ==
as.character(internal_list$info_variables$user_names_time_varying[,2])[i],
colnames(Psi) ==
as.character(internal_list$info_variables$user_names_time_varying[,1])[i]] <-
paste0("psi",
       "_",
as.character(internal_list$info_variables$user_names_time_varying[,1])[i],
       "_",
as.character(internal_list$info_variables$user_names_time_varying[,2])[i])
    }

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )

    #return output
    return(Psi)
  }

  #------------------------------------------
  # linear model with additive heterogeneity
  #------------------------------------------

  if(additive == TRUE &&
     linear == TRUE &&
     homogeneous == FALSE &&
     matrix_type == "Psi"){

    n_total <- internal_list$info_model$n_occasions *
      internal_list$info_model$n_processes +
      internal_list$info_model$n_time_invariant +
      internal_list$info_model$n_processes

    names_variables <-
      c(internal_list$info_variables$names_time_invariant_unobserved["user_names",],
        internal_list$info_variables$names_time_invariant_unique["user_names",],
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
    internal_list$info_variables$names_time_invariant_unique["user_names",]

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

  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )

  #return output
  return(Psi)
  }

  #############################################
  # MATRIX OF STRUCTURAL COEFFICIENTS
  #############################################

  #------------------------------------------
  # linear homogeneous model
  #------------------------------------------
  if(homogeneous == TRUE &&
     linear == TRUE &&
     matrix_type == "C" ){

    n_total <- internal_list$info_model$n_occasions *
      internal_list$info_model$n_processes +
      internal_list$info_model$n_time_invariant

    names_variables <-
      c(internal_list$info_variables$names_time_invariant_unique["user_names",],
        c(internal_list$info_variables$user_names_time_varying))

    C <- matrix(ncol = n_total,
                nrow = n_total)

    rownames(C) <- colnames(C) <- names_variables

    for (i in 1:nrow(internal_list$info_parameters$C_table)){

      row_name <- internal_list$info_parameters$C_table$incoming[i]
      col_name <- internal_list$info_parameters$C_table$outgoing[i]

      C[row_name,col_name] <- internal_list$info_parameters$C_table$value[i]

    }

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )

    #return output
    return(C)

  }

  #------------------------------------------
  # linear model with additive heterogeneity
  #------------------------------------------

  if(homogeneous == FALSE &&
     linear == TRUE &&
     additive  == TRUE &&
     matrix_type == "C" ){

    n_total <- internal_list$info_model$n_occasions *
      internal_list$info_model$n_processes +
      internal_list$info_model$n_time_invariant +
      internal_list$info_model$n_processes

    names_variables <-
      c(internal_list$info_variables$names_time_invariant_unobserved["user_names",],
        internal_list$info_variables$names_time_invariant_unique["user_names",],
        c(internal_list$info_variables$user_names_time_varying))

    C <- matrix(ncol = n_total,
                nrow = n_total)

    rownames(C) <- colnames(C) <- names_variables

    for (i in 1:nrow(internal_list$info_parameters$C_table)){

      row_name <- internal_list$info_parameters$C_table$incoming[i]
      col_name <- internal_list$info_parameters$C_table$outgoing[i]

      C[row_name,col_name] <- internal_list$info_parameters$C_table$value[i]

    }

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )

    #return output
    return(C)

  }

  #############################################
  # SELECTION MATRIX
  #############################################

  #------------------------------------------
  # linear homogeneous model
  #------------------------------------------



  #------------------------------------------
  # linear model with additive heterogeneity
  #------------------------------------------

  if(matrix_type == "selection" &&
     homogeneous == FALSE &&
     linear == TRUE &&
     additive  == TRUE){

      n_total <- internal_list$info_model$n_occasions *
        internal_list$info_model$n_processes +
        internal_list$info_model$n_time_invariant +
        internal_list$info_model$n_processes

      names_variables <-
        c(internal_list$info_variables$names_time_invariant_unobserved["user_names",],
          internal_list$info_variables$names_time_invariant_unique["user_names",],
          c(internal_list$info_variables$user_names_time_varying))

    seletion_matrix <- matrix(ncol = n_total,
                              nrow = n_total - internal_list$info_model$n_processes)

    selection_matrix_1 <- matrix(ncol = internal_list$info_model$n_processes,
                                 nrow = n_total - internal_list$info_model$n_processes)

    selection_matrix_1[, ] <- 0
    selection_matrix_2 <- diag(n_total - internal_list$info_model$n_processes)

    selection_matrix <- cbind(selection_matrix_1, selection_matrix_2)

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )

    #return output
    return(selection_matrix)

  }

}
