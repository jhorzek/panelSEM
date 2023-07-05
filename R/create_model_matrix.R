## Changelog:
# CG 0.0.2 2023-05-24: replaced arguments homogeneity and additive by heterogeneity
# CG 0.0.1 2023-02-20: initial programming

## Documentation
#' @title Create Model Matrices
#' @description Create model matrices such as the matrix of structural
#' coefficients, the covariance matrix, and selection matrix.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @param matrix_type Character string describing the type of model matrix.
#' Admissible values are: \code{"C"}, \code{"Psi"}, \code{"mean"}, and \code{"selection"}.
#' @return A matrix of model parameters or a selection matrix.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598


## Function definition
create_model_matrix <- function(internal_list,
                                matrix_type){

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

  linear <- internal_list$info_model$linear
  heterogeneity <- internal_list$info_model$heterogeneity

  #############################################
  # COVARIANCE MATRIX
  #############################################

  #------------------------------------------
  # linear homogeneous model
  #------------------------------------------

  if(linear == TRUE &&
     identical("homogeneous", sort(heterogeneity)) &&
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

    Psi_z[lower.tri(Psi_z)] <- t(Psi_z)[lower.tri(t(Psi_z))]

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

    Psi_init[lower.tri(Psi_init)] <- t(Psi_init)[lower.tri(t(Psi_init))]

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
      t(Psi_time_varying_contemporaneous)[lower.tri(t(Psi_time_varying_contemporaneous))]

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

  if(linear == TRUE &&
     identical("additive", sort(heterogeneity)) &&
     matrix_type == "Psi"){

    n_total <- internal_list$info_model$n_occasions *
      internal_list$info_model$n_processes +
      internal_list$info_model$n_time_invariant +
      internal_list$info_model$n_processes

    names_variables <-
      c(internal_list$info_variables$names_time_invariant_unobserved_additive["user_names",],
        internal_list$info_variables$names_time_invariant_unique["user_names",],
        c(internal_list$info_variables$user_names_time_varying))

  Psi <- matrix(ncol = n_total,
                nrow = n_total)

  rownames(Psi) <- colnames(Psi) <- names_variables

  ### (co-)variances among eta-variables
  Psi_eta <- matrix(ncol = internal_list$info_model$n_processes,
                    nrow = internal_list$info_model$n_processes)

  rownames(Psi_eta) <- colnames(Psi_eta) <-
    internal_list$info_variables$names_time_invariant_unobserved_additive["user_names",]

  Psi_eta[,] <-
    paste0("psi",
           "_",
           apply(expand.grid(rownames(Psi_eta), colnames(Psi_eta)),
                 1,
                 paste,
                 collapse = "_"))

  Psi_eta[lower.tri(Psi_eta)] <- t(Psi_eta)[lower.tri(t(Psi_eta))]

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

  Psi_z[lower.tri(Psi_z)] <- t(Psi_z)[lower.tri(t(Psi_z))]

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

  Psi_init[lower.tri(Psi_init)] <- t(Psi_init)[lower.tri(t(Psi_init))]

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

  #------------------------------------------
  # nonlinear model with nonadditive heterogeneitiy (cross-lagged)
  #------------------------------------------

  if(linear == FALSE &&
     "additive" %in% heterogeneity  &&
     "cross-lagged" %in% heterogeneity  &&
     matrix_type == "Psi"){

    # get dimension of matrix
    n_total <- internal_list$info_model$n_occasions *
      internal_list$info_model$n_processes +
      internal_list$info_model$n_time_invariant +
      internal_list$info_model$n_processes +
      internal_list$info_model$n_processes *
      (internal_list$info_model$n_processes - 1) +
      (internal_list$info_model$n_occasions - 1) *
      (internal_list$info_model$n_processes -1 ) *
      sum(lengths(internal_list$info_variables$info_time_invariant_variables))

    # get product term names

    # CG: get product term names from product data set
    # product_terms_names <- character(0)
    # TODO: delete if everything works
    #product_terms_names <- character(0)

    #for (i in 1:internal_list$info_model$n_processes){
    #  names_product <-
    #    as.vector(outer(
    #      internal_list$info_variables$info_time_invariant_variables[[i]],
    #      internal_list$info_variables$user_names_time_varying[
    #        i,
    #        -internal_list$info_model$n_occasions],
    #      paste,
    #      sep="*"))

    #  product_terms_names <- c(product_terms_names, names_product)
    #}

    product_terms_names <- colnames(internal_list$info_data$data_product_terms)

    # get matrix names
    names_variables <-
      c(internal_list$info_variables$names_time_invariant_unobserved_additive["user_names",],
        internal_list$info_variables$names_time_invariant_unobserved_cross_lagged["user_names",],
        product_terms_names,
        internal_list$info_variables$names_time_invariant_unique["user_names",],
        internal_list$info_variables$user_names_time_varying)

    # create empty covariance matrix
    Psi <- matrix(ncol = n_total,
                  nrow = n_total)

    rownames(Psi) <- colnames(Psi) <- names_variables

    Psi[,] <- "0"

    ### (co-)variances among eta-variables
    n_Psi_eta <- internal_list$info_model$n_processes +
      internal_list$info_model$n_processes *
      (internal_list$info_model$n_processes - 1)
    start_fill_Psi_eta <- 1
    end_fill_Psi_eta <- start_fill_Psi_eta + n_Psi_eta - 1

    Psi_eta <- matrix(ncol = n_Psi_eta,
                      nrow = n_Psi_eta)

    rownames(Psi_eta) <- colnames(Psi_eta) <- colnames(Psi)[start_fill_Psi_eta :
                                                              end_fill_Psi_eta]

    Psi_eta[,] <-
      paste0("psi",
             "_",
             apply(expand.grid(rownames(Psi_eta), colnames(Psi_eta)),
                   1,
                   paste,
                   collapse = "_"))

    Psi_eta[lower.tri(Psi_eta)] <- t(Psi_eta)[lower.tri(t(Psi_eta))]

    Psi[start_fill_Psi_eta : end_fill_Psi_eta,
        start_fill_Psi_eta : end_fill_Psi_eta] <- Psi_eta


    ### (co-)variances among observed product variables
    n_Psi_product <- (internal_list$info_model$n_occasions - 1) *
      (internal_list$info_model$n_processes -1 ) *
      sum(lengths(internal_list$info_variables$info_time_invariant_variables))
    start_fill_Psi_product <- end_fill_Psi_eta + 1
    end_fill_Psi_product <- start_fill_Psi_product + n_Psi_product - 1

    Psi_product <- matrix(ncol = n_Psi_product,
                          nrow = n_Psi_product)
    Psi_product[, ] <- "0"
    diag(Psi_product) <- "0.001"

    rownames(Psi_product) <- colnames(Psi_product) <-
      colnames(Psi)[start_fill_Psi_product : end_fill_Psi_product]


    # CG: the following code chunk produces a saturated covariance matrix and
    # is currently not needed
    #Psi_product[,] <-
    #  paste0("psi",
    #        "_",
    #         apply(expand.grid(rownames(Psi_product), colnames(Psi_product)),
    #               1,
    #               paste,
    #               collapse = "_"))

    #Psi_product[lower.tri(Psi_product)] <- Psi_product[upper.tri(Psi_product)]

    Psi[start_fill_Psi_product : end_fill_Psi_product,
        start_fill_Psi_product : end_fill_Psi_product] <- Psi_product

    ### (co-)variances among linear z-variables
    n_Psi_z <- internal_list$info_model$n_time_invariant
    start_fill_Psi_z <- end_fill_Psi_product + 1
    end_fill_Psi_z <- start_fill_Psi_z + n_Psi_z - 1

    Psi_z <-
      matrix(ncol = n_Psi_z ,
             nrow = n_Psi_z)

    rownames(Psi_z) <- colnames(Psi_z) <-
      colnames(Psi)[start_fill_Psi_z : end_fill_Psi_z]

    Psi_z[,] <-
      paste0("psi",
             "_",
             apply(expand.grid(rownames(Psi_z), colnames(Psi_z)),
                   1,
                   paste,
                   collapse = "_"))

    Psi_z[lower.tri(Psi_z)] <- t(Psi_z)[lower.tri(t(Psi_z))]

    Psi[start_fill_Psi_z : end_fill_Psi_z,
        start_fill_Psi_z : end_fill_Psi_z] <- Psi_z

    ### (co-)variances among initial variables
    n_Psi_init <- internal_list$info_model$n_processes
    start_fill_Psi_init <- end_fill_Psi_z + 1
    end_fill_Psi_init <- start_fill_Psi_init + n_Psi_init - 1

    Psi_init <- matrix(ncol = n_Psi_init,
                       nrow = n_Psi_init)

    rownames(Psi_init) <- colnames(Psi_init) <-
      colnames(Psi)[start_fill_Psi_init : end_fill_Psi_init]

    Psi_init[,] <-
      paste0("psi",
             "_",
             apply(expand.grid(rownames(Psi_init), colnames(Psi_init)),
                   1,
                   paste,
                   collapse = "_"))

    Psi_init[lower.tri(Psi_init)] <- t(Psi_init)[lower.tri(t(Psi_init))]

    Psi[start_fill_Psi_init : end_fill_Psi_init,
        start_fill_Psi_init : end_fill_Psi_init] <- Psi_init

    ### (co-)variances among NON-initial time-varying variables
    n_Psi_time_varying <- (internal_list$info_model$n_processes *
                             (internal_list$info_model$n_occasions - 1))
    start_fill_Psi_time_varying <- end_fill_Psi_init + 1
    end_fill_Psi_time_varying <- start_fill_Psi_time_varying + n_Psi_time_varying - 1

    Psi_time_varying <-
      matrix(ncol = n_Psi_time_varying,
             nrow = n_Psi_time_varying)

    Psi_time_varying[, ] <- "0"

    rownames(Psi_time_varying) <- colnames(Psi_time_varying) <-
      colnames(Psi)[start_fill_Psi_time_varying : end_fill_Psi_time_varying]

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

    Psi[start_fill_Psi_time_varying : end_fill_Psi_time_varying,
        start_fill_Psi_time_varying : end_fill_Psi_time_varying] <-
      Psi_time_varying

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
  if(linear == TRUE &&
     identical("homogeneous", sort(heterogeneity)) &&
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

  if(linear == TRUE &&
     identical("additive", sort(heterogeneity)) &&
     matrix_type == "C" ){

    n_total <- internal_list$info_model$n_occasions *
      internal_list$info_model$n_processes +
      internal_list$info_model$n_time_invariant +
      internal_list$info_model$n_processes

    names_variables <-
      c(internal_list$info_variables$names_time_invariant_unobserved_additive["user_names",],
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

  #------------------------------------------
  # nonlinear model with nonadditive heterogeneity (cross-lagged)
  #------------------------------------------

  if(linear == FALSE &&
     "additive" %in% heterogeneity &&
     "cross-lagged" %in% heterogeneity &&
     matrix_type == "C" ){

    # get dimension of matrix
    n_total <- internal_list$info_model$n_occasions *
      internal_list$info_model$n_processes +
      internal_list$info_model$n_time_invariant +
      internal_list$info_model$n_processes +
      internal_list$info_model$n_processes *
      (internal_list$info_model$n_processes - 1) +
      (internal_list$info_model$n_occasions - 1) *
      (internal_list$info_model$n_processes -1 ) *
      sum(lengths(internal_list$info_variables$info_time_invariant_variables))

    # get product term names

    # CG: get product term names from product data set
    # product_terms_names <- character(0)
    # TODO: delete if everything works
    #product_terms_names <- character(0)

    #for (i in 1:internal_list$info_model$n_processes){
    #  names_product <-
    #    as.vector(outer(
    #      internal_list$info_variables$info_time_invariant_variables[[i]],
    #      internal_list$info_variables$user_names_time_varying[
    #        i,
    #        -internal_list$info_model$n_occasions],
    #      paste,
    #      sep="*"))

    #  product_terms_names <- c(product_terms_names, names_product)
    #}

    product_terms_names <- colnames(internal_list$info_data$data_product_terms)

    # get matrix names
    names_variables <-
      c(internal_list$info_variables$names_time_invariant_unobserved_additive["user_names",],
        internal_list$info_variables$names_time_invariant_unobserved_cross_lagged["user_names",],
        product_terms_names,
        internal_list$info_variables$names_time_invariant_unique["user_names",],
        internal_list$info_variables$user_names_time_varying)

    C <- matrix(ncol = n_total,
                nrow = n_total)

    rownames(C) <- colnames(C) <- names_variables

    # adjust parameter table to fit the definition variable syntax in OpenMx
    C_table_OpenMx <- internal_list$info_parameters$C_table

    end_fill <- nrow(internal_list$info_parameters$C_table)
    number_entries <-
      (internal_list$info_model$n_occasions - 1) *
      (internal_list$info_model$n_processes -1 ) *
      internal_list$info_model$n_processes
    start_fill <- end_fill - number_entries + 1

    ## name outgoing variable
    matrix_labels <- matrix(nrow = internal_list$info_model$n_processes,
                            ncol = internal_list$info_model$n_processes)

    grid <-
      expand.grid(internal_list$info_variables$names_processes["user_names",],
                  internal_list$info_variables$names_processes["user_names",])

    matrix_labels[,] <-
      paste0("eta",
             apply(grid,
                   1,
                   paste,
                   collapse = ""))

    labels_vector <- character(0)

    for (i in 1:internal_list$info_model$n_processes){

      select_labels <- seq(1,internal_list$info_model$n_processes)[-i]
      add_labels <- matrix_labels[i, select_labels]

      labels_vector <- c(labels_vector, add_labels)

    }

    labels_outgoing <- rep(labels_vector,
                           each = ((internal_list$info_model$n_occasions - 1) *
                                     (internal_list$info_model$n_processes -1 )))


    C_table_OpenMx$outgoing[start_fill : end_fill] <- labels_outgoing

    ### value
    product_terms_names <- character(0)

    for (i in internal_list$info_model$n_processes:1){
      names_product <-
        as.vector(outer(
          labels_vector[(internal_list$info_model$n_processes + 1 - i)],
          internal_list$info_variables$user_names_time_varying[
            i,
            -internal_list$info_model$n_occasions],
          paste,
          sep="*"))

      product_terms_names <- c(product_terms_names, names_product)
    }

    C_table_OpenMx$value[start_fill : end_fill] <-
    paste0("data.",
           gsub(".*\\*",
                "",
                product_terms_names))


    # create matrix
    for (i in 1:nrow(C_table_OpenMx)){

      row_name <- C_table_OpenMx$incoming[i]
      col_name <- C_table_OpenMx$outgoing[i]

      C[row_name,col_name] <- C_table_OpenMx$value[i]

    }

    C[is.na(C)] <- "0"

    # CG: naming of C had changed above
    # TODO:delete if everything works
    # replace asteriks in row- and columnames of C matrix (OpenMX convention)
    #get_indeces <- function(x) {
    #  return ( grepl("*", x, fixed = TRUE) )
    #}

    #indexes_product <- unlist(lapply(rownames(C), FUN = get_indeces))

    #factor_1 <- gsub("\\*.*", "", rownames(C)[indexes_product])
    #factor_2 <- gsub(".*\\*", "", rownames(C)[indexes_product])

    #rownames(C)[indexes_product] <- colnames(C)[indexes_product] <-
    #  paste0("prod_",
    #       factor_1,
    #       "_",
    #       factor_2)

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )

    #return output
    return(C)

  }

  #############################################
  # MEAN VECTOR
  #############################################

  #------------------------------------------
  # nonlinear model with nonadditive heterogeneitiy (cross-lagged)
  #------------------------------------------

  if(linear == FALSE &&
     "additive" %in% heterogeneity  &&
     "cross-lagged" %in% heterogeneity  &&
     matrix_type == "mean"){

    # get dimension of vector
    n_total <- internal_list$info_model$n_occasions *
      internal_list$info_model$n_processes +
      internal_list$info_model$n_time_invariant +
      internal_list$info_model$n_processes +
      internal_list$info_model$n_processes *
      (internal_list$info_model$n_processes - 1) +
      (internal_list$info_model$n_occasions - 1) *
      (internal_list$info_model$n_processes -1 ) *
      sum(lengths(internal_list$info_variables$info_time_invariant_variables))

    n_obs <- ncol(internal_list$info_variables$names_time_invariant_unique) +
      internal_list$info_model$n_occasions * internal_list$info_model$n_processes

    n_latent <- n_total - n_obs

    variable_names <-
      c(internal_list$info_variables$names_time_invariant_unobserved_additive["user_names",],
        internal_list$info_variables$names_time_invariant_unobserved_cross_lagged["user_names",],
        colnames(internal_list$info_data$data_product_terms),
        internal_list$info_variables$names_time_invariant_unique["user_names",],
        internal_list$info_variables$user_names_time_varying)

    mean_vector <- paste0("m_",
                          variable_names)
    names(mean_vector) <- variable_names
    mean_vector[1 : (n_total - n_obs)] <- "0"


    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )

    #return output
    return(mean_vector)

  }

  #############################################
  # SELECTION MATRIX
  #############################################

  #------------------------------------------
  # linear homogeneous model
  #------------------------------------------

  # no selection matrix required since all variables are observed

  #------------------------------------------
  # linear model with additive heterogeneity
  #------------------------------------------

  if(matrix_type == "selection" &&
     linear == TRUE &&
     identical("additive", sort(heterogeneity))){

      n_total <- internal_list$info_model$n_occasions *
        internal_list$info_model$n_processes +
        internal_list$info_model$n_time_invariant +
        internal_list$info_model$n_processes

    seletion_matrix <- matrix(ncol = n_total,
                              nrow = n_total - internal_list$info_model$n_processes)

    selection_matrix_1 <- matrix(ncol = internal_list$info_model$n_processes,
                                 nrow = n_total - internal_list$info_model$n_processes)

    selection_matrix_1[, ] <- 0
    selection_matrix_2 <- diag(n_total - internal_list$info_model$n_processes)

    selection_matrix <- cbind(selection_matrix_1, selection_matrix_2)

    colnames(selection_matrix) <-
      c(internal_list$info_variables$names_time_invariant_unobserved_additive["user_names",],
        internal_list$info_variables$names_time_invariant_unique["user_names",],
        c(internal_list$info_variables$user_names_time_varying))

    rownames(selection_matrix) <-
      c(internal_list$info_variables$names_time_invariant_unique["user_names",],
        internal_list$info_variables$user_names_time_varying)

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )

    #return output
    return(selection_matrix)

  }

  #------------------------------------------
  # nonlinear model with nonadditive heterogeneitiy (cross-lagged)
  #------------------------------------------

  if(linear == FALSE &&
     "additive" %in% heterogeneity  &&
     "cross-lagged" %in% heterogeneity  &&
     matrix_type == "selection"){

    # get dimension of matrix
    n_total <- internal_list$info_model$n_occasions *
      internal_list$info_model$n_processes +
      internal_list$info_model$n_time_invariant +
      internal_list$info_model$n_processes +
      internal_list$info_model$n_processes *
      (internal_list$info_model$n_processes - 1) +
      (internal_list$info_model$n_occasions - 1) *
      (internal_list$info_model$n_processes -1 ) *
      sum(lengths(internal_list$info_variables$info_time_invariant_variables))

    n_obs <- ncol(internal_list$info_variables$names_time_invariant_unique) +
    internal_list$info_model$n_occasions * internal_list$info_model$n_processes

    n_latent <- n_total - n_obs

    seletion_matrix <- matrix(ncol = n_total,
                              nrow = n_obs)

    selection_matrix_1 <- matrix(ncol = n_latent,
                                 nrow = n_obs)

    selection_matrix_1[, ] <- 0
    selection_matrix_2 <- diag(n_obs)

    selection_matrix <- cbind(selection_matrix_1, selection_matrix_2)

    colnames(selection_matrix) <-
      c(internal_list$info_variables$names_time_invariant_unobserved_additive["user_names",],
        internal_list$info_variables$names_time_invariant_unobserved_cross_lagged["user_names",],
        colnames(internal_list$info_data$data_product_terms),
        internal_list$info_variables$names_time_invariant_unique["user_names",],
        internal_list$info_variables$user_names_time_varying)
    rownames(selection_matrix) <-
      c(internal_list$info_variables$names_time_invariant_unique["user_names",],
        internal_list$info_variables$user_names_time_varying)


    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )

    #return output
    return(selection_matrix)

  }

}
