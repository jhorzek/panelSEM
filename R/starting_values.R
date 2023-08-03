## Changelog:
# CG  0.0.6 2023-06-07: defined objects variables_tv and variables_ti for
#                       the function call of auxiliary models
# CG  0.0.5 2023-05-24: removed OpenMx == TRUE part for linear additive model
# CG  0.0.4 2023-05-24: replaced arguments homogeneity and additive by heterogeneity
# CG  0.0.3 2023-04-24 change argument to internal_list
# CDM 0.0.2 2023-03-01 add OpenMx part for certain starting values
# CG  0.0.1 2023-02-06 initial programming

## Documentation
#' @title Compute Starting Values
#' @description Compute starting values based on simplified auxiliary models
#' (e.g., models without latent variables) which are similar to the target model
#' of interest.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted list with the column starting values filled in to the
#' parameter following lists: \code{..$info_parameters$parameter_list_C} and
#' \code{..$info_parameters$parameter_list_Psi}.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598

starting_values <- function(internal_list){

  # function name
  fun.name <- "starting_values"

  # function version
  fun.version <- "0.0.3 2023-04-24"

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

  # extract model information from the arguments
  # TODO: check if we need to define all these objects or if we can directly
  # refer to the internal list
  data <- internal_list$info_data$data

  n_occasions <- internal_list$info_model$n_occasions

  n_processes <- internal_list$info_model$n_processes

  labels_time_varying_variables <-
    as.vector(t(internal_list$info_variables$user_names_time_varying))

  labels_time_invariant_variables <-
    internal_list$info_variables$info_time_invariant_variables

  linear <- internal_list$info_model$linear
  heterogeneity <- internal_list$info_model$heterogeneity
  verbose <- internal_list$control$verbose
  use_open_mx <- internal_list$info_model$use_open_mx

  parameter_list_C <- internal_list$info_parameters$C_table
  parameter_list_Psi <- internal_list$info_parameters$Psi_table

  #------------------------------------------
  # linear homogeneous model
  #------------------------------------------

  if(linear == TRUE &&
     identical("homogeneous", sort(heterogeneity)) ){

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )
    # return output
    return(internal_list)
  }

  #------------------------------------------
  # linear model with additive heterogeneity
  #------------------------------------------

  if(linear == TRUE &&
     identical("additive", sort(heterogeneity))){

    if (is.null(labels_time_invariant_variables)){

      # This part is for the model WITHOUT Z-variables. Let's fill in the
      # "else"-part (i.e., the model WITH Z-variables) below first and
      # then copy paste parts of it into the "if" part.

    } else {

      parameter_list_Psi$start_1 <- NA
      parameter_list_C$start_1 <- NA

      labels_processes <-
        internal_list$info_variables$names_processes["user_names",]

      labels_time <- vector("list", length = n_occasions)

      for (i in 1:n_occasions){
        select <- seq(from = i, by = n_occasions, length.out = n_processes)
        labels_time[[i]] <- labels_time_varying_variables[select]
      }


      # residualize the variables
      reg_models <- vector("list", length = length(labels_time_varying_variables))

      for (j in 1:n_processes) {
        reg_models[[j]] <-
          paste0(labels_time[[1]][j], " ~ ",
                 paste(unique(unlist(labels_time_invariant_variables)),
                       collapse = " + "))
      }

      for (j in 2:n_occasions){
        for (i in 1:n_processes) {

          reg_models[[(j-1)*n_processes+i]] <-
            paste0(labels_time[[j]][i],
                   " ~ ",
                   paste(labels_time[[j-1]], collapse = " + "),
                   " + ",
                   paste(labels_time_invariant_variables[[i]], collapse = " + "))
        }
      }

      fit_lm <- function(x){
        lm_fit <- stats::lm(x , data = data)
        return(stats::residuals(lm_fit))
      }

      fit_lm_list <- lapply(reg_models, FUN =fit_lm)
      resid_data_frame <- as.data.frame(do.call(cbind, fit_lm_list))
      colnames(resid_data_frame) <- paste0(unlist(labels_time),"_res")

      #TODO: change d to data
      d <- cbind(data,resid_data_frame)

      # compute starting values of covariances of eta
      # TODO: Delete: parameter_list$start_values <- NA

      #################################
      # COVARIANCES AMONG ETA-VARIABLES
      #################################

      ## compute person-specific means (EXCLUDING the initial value)
      ## for each process
      mean_no_init <- data.frame(matrix(nrow = nrow(d), ncol = n_processes))

      for (i in 1:n_processes){
        mean_no_init[,i] <-
          apply(d[, paste0(unlist(lapply(labels_time, `[[`, i)[-1]),"_res")],
                1,
                mean)
      }

      colnames(mean_no_init) <-
        paste0(unlist(labels_processes),"_mean_no_init")

      cov_mean_no_init_values <- stats::cov(mean_no_init)
      cov_mean_no_init_labels <- matrix (nrow = n_processes,
                                         ncol = n_processes)

      ## add starting values to parameter table
      for (i in 1:(n_processes)){
        # inner loop
        j <- 1
        while (j <= i){
          # assign label
          cov_mean_no_init_labels[j,i]     <-
            paste0("psi_eta",
                   paste0(labels_processes[j]),
                   "_eta",
                   paste0(labels_processes[i])
            )
          j <- j + 1
        }
      }

      param_cov_eta <- data.frame(labels = OpenMx::vech(t(cov_mean_no_init_labels)),
                                  values = OpenMx::vech(cov_mean_no_init_values))

      for(i in param_cov_eta$labels){
        parameter_list_Psi$start_1[parameter_list_Psi$value == i] <-
          param_cov_eta$values[param_cov_eta$labels == i]
      }

      #####################################
      # COVARIANCES AMONG INITIAL VARIABLES
      #####################################
      ## compute person-specific mean (INCLUDING the initial value) for each process
      for (i in 1:n_processes){
        k <- ncol(d)
        d[,k+1] <-
          apply(d[, paste0(unlist(lapply(labels_time, `[[`, i)),"_res")],
                1,
                mean)
      }

      colnames(d)[(ncol(d)-n_processes+1):ncol(d)] <-
        paste0(labels_processes,"_mean")

      ## create subset of data with relevant variables only
      data_cov_init <- d[, c(paste0(labels_time[[1]],"_res"),
                             paste0(labels_processes,"_mean") )]

      ## create starting value model
      model_cov_init <- vector("character",
                               length = (2*n_processes-1))

      for (i in 1:n_processes){
        model_cov_init[i] <-
          paste0(colnames(data_cov_init)[i],
                 " ~ ",
                 paste0(colnames(data_cov_init)[(n_processes+1):ncol(data_cov_init)],
                        collapse = " + ")
          )
      }

      for (j in 1:(n_processes-1)){
        model_cov_init[[n_processes+j]] <-
          paste0(colnames(data_cov_init)[j],
                 " ~~ ",
                 paste0(colnames(data_cov_init)[(j+1):(ncol(data_cov_init)-n_processes)],
                        collapse = " + "))
      }

      ## fit starting value model
      joint_reg_initial <- paste0(model_cov_init, collapse = "\n")
      fit_joint_reg_initial <- lavaan::sem(joint_reg_initial, data = data_cov_init)

      ## add starting values to parameter table
      par_joint_reg_initial <-
        lavaan::parTable(fit_joint_reg_initial)

      for (i in 1:(n_processes)){
        # inner loop
        j <- 1
        while (j <= i){
          # assign label
          par_joint_reg_initial$label[
            which(par_joint_reg_initial$lhs == colnames(data_cov_init)[j] &
                    par_joint_reg_initial$op ==  "~~" &
                    par_joint_reg_initial$rhs == colnames(data_cov_init)[i]  )
          ] <-
            paste0("psi_",
                   gsub("_.*", "", colnames(data_cov_init)[j]),
                   "_",
                   gsub("_.*", "", colnames(data_cov_init)[i]))
          j <- j + 1
        }
      }

      for(i in par_joint_reg_initial$label){
        parameter_list_Psi$start_1[parameter_list_Psi$value == i] <-
          par_joint_reg_initial$est[par_joint_reg_initial$label == i]
      }

      #################################################################
      # STRUCTURAL COEFFICIENTS OF ETA-VARIABLES ONTO INITIAL VARIABLES
      #################################################################

      ## create starting value model
      ### regressions
      model_init_eta_reg_process_1 <- vector("character", length = n_processes)

      for (i in 1:n_processes){
        model_init_eta_reg_process_1[i] <-
          paste0("eta",
                 labels_processes[i],
                 " =~ ",
                 "NA * ",
                 labels_time[[1]][1])
      }

      model_init_eta_reg_processes <-
        vector("list",
               length =  n_processes)

      for (i in 1:n_processes){
        for (j in 1:(n_processes-1)){
          model_init_eta_reg_processes[[i]][j] <-
            paste0("c_",
                   labels_time[[1]][j+1],
                   "_eta",
                   labels_processes[i],
                   " * ",
                   labels_time[[1]][j+1])
        }
      }

      fun_init_eta_reg_processes <- function(x){
        paste0(x, collapse = " + ")
      }

      model_init_eta_reg_processes <-
        unlist(lapply(model_init_eta_reg_processes,
                      FUN = fun_init_eta_reg_processes))

      model_init_eta_reg_non_init <- vector("character", length = n_processes)

      for (i in 1:n_processes){
        model_init_eta_reg_non_init[i] <-
          paste0("1 * ",
                 paste0(unlist(lapply(labels_time, `[[`, i)),"_res")[-1],
                 collapse = " + ")
      }

      ######## lines with eta
      model_init_eta_cov_eta <- vector("character", length = nrow(param_cov_eta))

      for (i in 1:nrow(param_cov_eta)){
        model_init_eta_cov_eta[i] <-
          paste0(gsub("\\_.*", "",gsub('psi_', '', param_cov_eta$labels[i])),
                 " ~~ ",
                 param_cov_eta$values[i],
                 " * ",
                 gsub(".*\\_", "",gsub('psi_', '', param_cov_eta$labels[i])))
      }

      ######## lines with initial variables
      cov_init_labels <- matrix (nrow = n_processes,
                                 ncol = n_processes)

      for (i in 1:(n_processes)){
        # inner loop
        j <- 1
        while (j <= i){
          # assign label
          cov_init_labels[j,i]     <-
            paste0("psi_",
                   paste0(labels_time[[1]][j]),
                   "_",
                   paste0(labels_time[[1]][i])
            )
          j <- j + 1
        }
      }

      param_cov_init <- data.frame(labels = OpenMx::vech(t(cov_init_labels)))
      param_cov_init$values <-
        parameter_list_Psi$start_1[which(parameter_list_Psi$value %in%
                                           param_cov_init$labels)]

      model_init_eta_cov_init <- vector("character", length = nrow(param_cov_init))

      for (i in 1:nrow(param_cov_init)){
        model_init_eta_cov_init[i] <-
          paste0(gsub("\\_.*", "",gsub('psi_', '', param_cov_init$labels[i])),
                 " ~~ ",
                 param_cov_init$values[i],
                 " * ",
                 gsub(".*\\_", "",gsub('psi_', '', param_cov_init$labels[i])))
      }

      model_init_eta <-
        rbind(model_init_eta_reg_process_1,
              model_init_eta_reg_processes,
              model_init_eta_reg_non_init)

      row.names(model_init_eta) <- NULL

      model_init_eta <-
        paste0(apply(model_init_eta, 2 ,FUN = fun_init_eta_reg_processes),
               collapse = "\n")

      model_init_eta_cov_eta  <-
        paste0(model_init_eta_cov_eta, collapse = "\n")

      model_init_eta_cov_init <-
        paste0(model_init_eta_cov_init, collapse = "\n")

      model_init_eta <-
        paste0(c(model_init_eta, model_init_eta_cov_eta, model_init_eta_cov_init),
               collapse = "\n")

      ## fit starting value model
      fit_model_init_eta <- lavaan::sem(model_init_eta, data = d)

      ## add starting values to parameter table
      par_fit_model_init_eta <-
        lavaan::parTable(fit_model_init_eta)

      for (i in 1:(n_processes)){
        par_fit_model_init_eta$label[
          which(par_fit_model_init_eta$lhs == paste0("eta",labels_processes[i]) &
                  par_fit_model_init_eta$op ==  "=~" &
                  par_fit_model_init_eta$rhs == labels_time[[1]][1]  )
        ] <-
          paste0("c_",
                 labels_time[[1]][1],
                 "_eta",
                 labels_processes[i])
      }

      for(i in par_fit_model_init_eta$label){
        parameter_list_C$start_1[parameter_list_C$value == i] <-
          par_fit_model_init_eta$est[par_fit_model_init_eta$label == i]
      }

      ###############################################################
      # STRUCTURAL COEFFICIENTS OF Z-VARIABLES ONTO INITIAL VARIABLES
      ###############################################################

      ## create starting value model
      reg_models_init_z <- vector("list", length = n_processes)

      for (i in 1:n_processes) {

        reg_models_init_z[i] <-
          paste0(labels_time[[1]][i],
                 " ~ ",
                 paste0(unique(unlist(labels_time_invariant_variables)),
                        collapse = " + "),
                 " + ",
                 paste0(colnames(mean_no_init)[i]))
      }

      ## fit starting value model
      ### create subset of data with relevant variables only
      data_init_z <-
        cbind(d[, c(labels_time[[1]],
                    unique(unlist(labels_time_invariant_variables)))],
              mean_no_init)

      fit_lm_init_z <- function(x){
        lm_fit <- stats::lm(x , data = data_init_z)
        return(stats::coef(lm_fit))
      }

      fit_lm_init_z_list <- lapply(reg_models_init_z, FUN =fit_lm_init_z)
      names(fit_lm_init_z_list) <- labels_time[[1]]

      for (i in labels_time[[1]]){
        for (j in unique(unlist(labels_time_invariant_variables))){
          parameter_list_C$start_1[
            parameter_list_C["value"] ==
              paste0("c_",
                     paste0(i),
                     "_",
                     paste0(j))] <- fit_lm_init_z_list[[i]][j]
        }
      }

      ##################################################################
      # MODEL FOR
      #  - (cross-) lagged coefficients
      #  - coefficients of z-variables onto non-initial variables
      #  - the (co)-variances of the the noninitial time-varying observed
      #  - variables
      ###################################################################

      variables_tv <- vector("list", length = internal_list$info_model$n_processes)

      for (i in 1:internal_list$info_model$n_processes){
        variables_tv[[i]] <- internal_list$info_variables$user_names_time_varying[i,]
      }

      variables_ti <- vector("list", length = internal_list$info_model$n_processes)

      for (i in 1:internal_list$info_model$n_processes){
        variables_ti[[i]] <- internal_list$info_variables$info_time_invariant_variables[[i]]
      }

      internal_list_aux <-
        auxiliary_model(time_varying_variables = variables_tv,
                        time_invariant_variables = variables_ti,
                        linear = linear,
                        heterogeneity  = "homogeneous",
                        use_open_mx = use_open_mx,
                        verbose = verbose)

      model_hom <- internal_list_aux$model_syntax$lavaan
      fit_hom <- lavaan::sem(model_hom,
                             data = data)

      coef_lav_hom <- lavaan::coef(fit_hom)
      coef_lav_hom_labels <- unique(names(coef_lav_hom))
      coef_lav_hom <- coef_lav_hom[coef_lav_hom_labels]


      ## fill in starting values for structural coefficients
      for (i in names(coef_lav_hom)){
        select_row <- which(parameter_list_C$value == i)

        if (all(is.na(parameter_list_C$start_1[select_row]))){
          parameter_list_C$start_1[select_row] <-
            coef_lav_hom[names(coef_lav_hom) == i]
        }
      }

      ## fill in starting values for covariance matrix
      for (i in names(coef_lav_hom)){
        select_row <- which(internal_list$info_parameters$Psi_table$value == i)

        if (all(is.na(parameter_list_Psi$start_1[select_row]))){
          parameter_list_Psi$start_1[select_row] <-
            coef_lav_hom[names(coef_lav_hom) == i]
        }
      }

    }

    # prepare output

    internal_list$info_parameters$C_table <- parameter_list_C
    internal_list$info_parameters$Psi_table <- parameter_list_Psi

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )
    # return output
    return(internal_list)
  }

  #------------------------------------------
  # nonlinear model with nonadditive heterogeneity (cross-lagged)
  #------------------------------------------

  if(linear == FALSE &&
     "additive" %in% heterogeneity  &&
     "cross-lagged" %in% heterogeneity){

    # console output
    if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                    Sys.time(), "\n" ) )
    # return output
    return(internal_list)

  }

}




