## Changelog:
# CG 0.0.1 2023-03-23: initial programming

## Documentation
#' @title Fit Panel Data Model
#' @description Fit a model from the class of dynamic panel data models to longitudinal data.
#' Models include linear and nonlinear cross-lagged panel models with additive or nonadditive
#' unobserved heterogeneity.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @param rand Logical value indicating if (\code{TRUE}) random starting values
#' should be chosen.
#' @return An object of class \code{panelSEM} for which several methods
#' are available including \code{\link{summary.panelSEM}} and
#' \code{\link{print.panelSEM}}.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87,
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @references Gische, C., West, S.G., & Voelkle, M.C. (2021) Forecasting Causal
#'  Effects of Interventions versus Predicting Future Outcomes, Structural
#'  Equation Modeling: A Multidisciplinary Journal, 28:3, 475-492,
#'  DOI: 10.1080/10705511.2020.1780598
#' @keywords external

fill_in_estimates <- function(internal_list,
                              rand=FALSE){

  psem.matA <- internal_list$model_matrices$C_labels  ##coef labels
  psem.matS <- internal_list$model_matrices$Psi_labels ## var-cov labels
  psem.matF <- internal_list$model_matrices$select_observed_only ## filter matrix
  var_names <- colnames(psem.matA)
  obs_var <- internal_list$info_data$var_names
  nvar <- length(var_names)
  nobs <- length(obs_var)



  labelsA <- as.vector(t(psem.matA)) ## label vector
  labelsA[which(labelsA=="1")] <- "xxx"  ### "1" is not a valid label
  if(rand){
    indicA <- ifelse(is.na(psem.matA)==TRUE,0,ifelse(psem.matA=="1",1, sample(seq(-5,5, by=0.1), length(is.na(psem.matA)==FALSE), replace = T) )) ## random selection of starting values
    indm <- expand.grid(1:nvar, 1:nvar)
    for(i in 1:nrow(indm)){
      curr_pos <- unlist(indm[i,])
      flag_to_rep <- !is.na(psem.matA[curr_pos[1], curr_pos[2]]) && !psem.matA[curr_pos[1], curr_pos[2]]== "1"
      if(flag_to_rep) {
        indvec <-  which(psem.matA==psem.matA[curr_pos[1], curr_pos[2]])
        if(length(indvec)>1) indicA[indvec] <- sample(seq(0.5,3, by=0.1),1)
      }
    } ## randomly select starting values for the coefficients taking into account equality constraints

  }
  else{
    indicA <- ifelse(is.na(psem.matA)==TRUE,0,1)
  }

  valuesA <- as.vector(t(indicA))

  freeA <- ifelse(is.na(psem.matA)==TRUE | psem.matA=="1", F, T)
  freeA_vec <- as.vector(t(freeA))



  labelsS <- as.vector(t(psem.matS))
  if(rand){
    indicS <- ifelse(is.na(psem.matS)==TRUE,0,sample(seq(0.5,10, by=0.1), length(is.na(psem.matS)==FALSE), replace = T)) ## random selection of starting values
    indicS[lower.tri(indicS)] = t(indicS)[lower.tri(indicS)] ##makes the matrix symmetric

    indm <- expand.grid(1:nvar, 1:nvar)
    for(i in 1:nrow(indm)){
      curr_pos <- unlist(indm[i,])
      flag_to_rep <- !is.na(psem.matS[curr_pos[1], curr_pos[2]])
      if(flag_to_rep) {
        indvec <-  which(psem.matS==psem.matS[curr_pos[1], curr_pos[2]])
        if(length(indvec)>1) indicS[indvec] <- sample(seq(0.5,3, by=0.1),1)
      }
    } ## randomly select starting values for the (co)variances taking into account equality constraints
  }
  else{
    indicS <- ifelse(is.na(psem.matS)==TRUE,0,1)
  }

  valuesS <- as.vector(t(indicS))
  freeS <- ifelse(is.na(psem.matS)==TRUE, F, T)
  freeS_vec <- as.vector(t(freeS))

  valuesF <- as.vector(t(psem.matF))

  ##openMx model set-up

  raw_data <- mxData(observed = data, type = 'raw')
  matrA <- mxMatrix( type="Full", nrow=nvar, ncol=nvar,
                     free=freeA_vec, values=valuesA,
                     labels=labelsA, byrow=TRUE, name="A", dimnames = list(var_names,var_names) )
  matrS <- mxMatrix( type="Symm", nrow=nvar, ncol=nvar,
                     free=freeS_vec, values=valuesS,
                     labels=labelsS, byrow=TRUE, name="S", dimnames = list(var_names,var_names) )
  matrF <- mxMatrix( type="Full", nrow=nobs, ncol=nvar, name="F", values = valuesF, free = FALSE,
                     dimnames = list(NULL,var_names), byrow = T)
  matrM <- mxMatrix( type="Full", nrow=1, ncol=nvar,
                     free=c(rep(F,nvar-nobs),rep(T,nobs)), values=rep(0,nvar),
                     name="M", labels = paste0('mean_', var_names),
                     dimnames = list(NULL, var_names)) ### free mean
  expRAM <- mxExpectationRAM("A","S","F","M", dimnames=var_names)
  funML <- mxFitFunctionML()
  lin_add_mod <- mxModel("linear additive OpenMx model",
                         raw_data, matrA, matrS, matrF, matrM, expRAM, funML)
  status <- FALSE
  while(!status){
    lin_add_fit <- mxTryHard(lin_add_mod)
    if( lin_add_fit$output$status$code %in% c(0:1)) status <- TRUE
  }

  return(summary(lin_add_fit))

}
