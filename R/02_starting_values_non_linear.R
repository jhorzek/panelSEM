###########################
#
# PART I DATA GENERATION
#
###########################


library(Matrix)
library(matrixcalc)
library(MASS)
library(lavaan)
library(OpenMx)
mxOption(NULL, 
         'Number of Threads', 
         4)

# Model 
N <- 1000 # number of individuals
t <- 6  # number of time points 
k <- 2 # number of time-varying processes
k_z <- 3 # TOTAL number of observed time-invariant variables
k_eta <- 4 # TOTAL number of random coefficients

# Set seed
set.seed(89435)


# Variable names
labels_id <- c(24:25,23:1)

## time-varying variables (including initial variables)
names_time_varying <- paste0(letters[labels_id[1:k]],
                             rep(1:t, each = k))

## observed time-invariant variables
names_time_invariant <- paste0("z", 1:k_z)

## unobserved time-invariant variables 
# TODO Fill in labels


####################################
# Mean Vector of exogenous variables
####################################
mu_epsilon_eta <- rep(0, k_eta)
mu_epsilon_z <- rep(0, k_z)
mu_epsilon_init <- rep(0, k)
mu_epsilon_time_varying <- rep(0, k*(t-1))
mu <- c(mu_epsilon_eta, 
        mu_epsilon_z, 
        mu_epsilon_init, 
        mu_epsilon_time_varying)

###########################################
# Covariances Matrix of Exogenous Variables
###########################################
# covariance-matrix of the epsilon_eta variables
A_sigma_eta <- matrix(nrow = k_eta, ncol = k_eta, byrow = T,
                      c(1, 0.5, 0.25, 0.125,
                        1.5, 0.5, 0.25, 0.125,
                        1, 0.25, 0.25, 0.125,
                        1, 1, 0.5, 0.125))
sigma_epsilon_eta <- t(A_sigma_eta) %*% A_sigma_eta
is.positive.definite(sigma_epsilon_eta)
psi_eta <- c(
  "psi_etax_etax" = sigma_epsilon_eta[1,1], 
  "psi_etax_etay" = sigma_epsilon_eta[1,2],
  "psi_etax_etaxy" = sigma_epsilon_eta[1,3],
  "psi_etax_etayx" = sigma_epsilon_eta[1,4],
  "psi_etay_etay" = sigma_epsilon_eta[2,2], 
  "psi_etay_etaxy" = sigma_epsilon_eta[2,3],
  "psi_etay_etayx" = sigma_epsilon_eta[2,4],
  "psi_etaxy_etaxy" = sigma_epsilon_eta[3,3],
  "psi_etaxy_etayx" = sigma_epsilon_eta[3,4],
  "psi_etayx_etayx" = sigma_epsilon_eta[4,4]
)

# covariance-matrix of the epsilon_z variables
A_sigma_z <- matrix(nrow = k_z, ncol = k_z, byrow = T,
                    c(1, 0.75, 0.3, 0.75, 1, 0.25, 0.3, 0.25, 1))
sigma_epsilon_z <- t(A_sigma_z) %*% A_sigma_z
psi_z <- c(
  "psi_z1_z1" = sigma_epsilon_z[1,1], 
  "psi_z1_z2" = sigma_epsilon_z[1,2],
  "psi_z1_z3" = sigma_epsilon_z[1,3],
  "psi_z2_z2" = sigma_epsilon_z[2,2],
  "psi_z2_z3" = sigma_epsilon_z[2,3],
  "psi_z3_z3" = sigma_epsilon_z[3,3]
)
is.positive.definite(sigma_epsilon_z)

# covariance matrix of the epsilon-variables related to the initial variables
A_sigma_eps_init <- matrix(nrow = k, ncol = k, c(1, 0.3, 0.3, 1))
sigma_epsilon_init <- t(A_sigma_eps_init) %*% A_sigma_eps_init
psi_init <- c(
  "psi_x1_x1" = sigma_epsilon_init[1,1], 
  "psi_x1_y1" = sigma_epsilon_init[1,2],
  "psi_y1_y1" = sigma_epsilon_init[2,2]
)
is.positive.definite(sigma_epsilon_init)

# covariance matrix of the epsilon-variables of (non-initial) time-varying 
# variables
A_sigma_eps <- diag(sqrt(3), nrow = k*(t-1), ncol = k*(t-1))
sigma_epsilon_time_varying <- t(A_sigma_eps) %*% A_sigma_eps
psi_time_varying <- c(
  "psi_x_x" = sigma_epsilon_time_varying[1,1], 
  "psi_y_y" = sigma_epsilon_time_varying[2,2]
)
is.positive.definite(sigma_epsilon_time_varying)

# Psi Matrix
psi <- as.matrix(bdiag(sigma_epsilon_eta, 
                       sigma_epsilon_z, 
                       sigma_epsilon_init, 
                       sigma_epsilon_time_varying))

# dim(psi)
# psi
is.positive.definite(psi)

######################################
# Matrix of Structural Coefficients
######################################

# edges pointing to x1
c_x1_etax = -2
c_x1_etay = 1
c_x1_etaxy <- 1
c_x1_etayx <- 0.5
c_x1_z1 = -0.3
c_x1_z2 = -0.2
c_x1_z3 = 1


# edges pointing to y1
c_y1_etax = -2.5
c_y1_etay = 1.5
c_y1_etaxy <- 1
c_y1_etayx <- 0.5
c_y1_z1 = - 0.8
c_y1_z2 = 0.2
c_y1_z3 = 1

# edges pointing to x_s, s=2,...,t
c_x_z1 <- 0.5
c_x_z2 <- 2
c_x_x <- 0.05
c_x_y <- 0.4
c_x_y_z1 <- -0.5
c_x_y_z2 <- 1

# edges pointing to y_s, s=2,...,t
c_y_z2 <- 1.5
c_y_z3 <- 2
c_y_x <- -0.6
c_y_y <- 1.2
c_y_x_z2 <- 1.5
c_y_x_z3 <- 2

##########################################
# Create Table of Model Parameters
##########################################
# structural coefficients onto initial variables
population_parameters <- c(
  "c_x1_etax" = c_x1_etax, 
  "c_x1_etay" = c_x1_etay,
  "c_x1_etaxy" = c_x1_etaxy, 
  "c_x1_etayx" = c_x1_etayx, 
  "c_x1_z1" = c_x1_z1, 
  "c_x1_z2" = c_x1_z2, 
  "c_x1_z3" = c_x1_z3,
  "c_y1_etax" = c_y1_etax, 
  "c_y1_etay" = c_y1_etay, 
  "c_y1_etaxy" = c_y1_etaxy,
  "c_y1_etayx" = c_y1_etayx,
  "c_y1_z1" = c_y1_z1,
  "c_y1_z2" = c_y1_z2, 
  "c_y1_z3" = c_y1_z3)

# structural coefficients onto non-initial variables
population_parameters <- c(
  population_parameters,
  "c_x_x" = c_x_x, 
  "c_x_y" = c_x_y,
  "c_x_z1" = c_x_z1, 
  "c_x_z2" = c_x_z2,
  "c_x_y_z1" = c_x_y_z1,
  "c_x_y_z2" = c_x_y_z2,
  "c_y_x" = c_y_x, 
  "c_y_y" = c_y_y,
  "c_y_z2" = c_y_z2, 
  "c_y_z3" = c_y_z3, 
  "c_y_x_z2" = c_y_x_z2,
  "c_y_x_z3" = c_y_x_z3)

# variance-covariance parameters eta-variables
population_parameters <- c(
  population_parameters,
  psi_eta, 
  psi_z, 
  psi_init,
  psi_time_varying)

population_parameters <- as.data.frame(population_parameters)

##############################
# Simulate Exogenous Variables
##############################
# draw from the distribution of exogenous variables
unobserved_exogenous <- mvrnorm(N, mu, psi)

# create data frame with simulated values of exogenous variables
data_unobserved_exogenous <- as.data.frame(unobserved_exogenous)
names_time_varying_error <- paste0("eps_",
                                   names_time_varying)

names_time_invariant_error <- paste0("eps_",
                                     names_time_invariant)

colnames_unobserved_exogenous <-  c("eps_etax",
                                    "eps_etay",
                                    "eps_etaxy",
                                    "eps_etayx",
                                    names_time_invariant_error,
                                    names_time_varying_error)

colnames(data_unobserved_exogenous) <- colnames_unobserved_exogenous

id_wide <- 1:N
d_wide <- cbind(id = id_wide, data_unobserved_exogenous)

###############################
# Simulate Endogenous Variables
###############################
# endogenous unobserved variables
d_wide$etax <- 1.0 * d_wide$eps_etax
d_wide$etay <- 1.0 * d_wide$eps_etay
d_wide$etaxy <- 1.0 * d_wide$eps_etaxy
d_wide$etayx <- 1.0 * d_wide$eps_etayx

# endogenous observed variables: time-invariant variables
d_wide$z1 <- 1.0 * d_wide$eps_z1
d_wide$z2 <- 1.0 * d_wide$eps_z2
d_wide$z3 <- 1.0 * d_wide$eps_z3

# endogenous observed variables: initial time-varying variables
d_wide$x1 <- 
  c_x1_etax * d_wide$etax +
  c_x1_etay * d_wide$etay +
  c_x1_etaxy * d_wide$etaxy +
  c_x1_etayx * d_wide$etayx +
  c_x1_z1 * d_wide$z1 +
  c_x1_z2 * d_wide$z2 +
  c_x1_z3 * d_wide$z3 +
  1 * d_wide$eps_x1

d_wide$y1 <- 
  c_y1_etax * d_wide$etax +
  c_y1_etay * d_wide$etay +
  c_y1_etaxy * d_wide$etaxy + 
  c_y1_etayx * d_wide$etayx + 
  c_y1_z1 * d_wide$z1 +
  c_y1_z2 * d_wide$z2 +
  c_y1_z3 * d_wide$z3 +
  1 * d_wide$eps_y1

# endogenous observed variables: non-initial time-varying variables
d_observed <- matrix(0, ncol = k*(t-1), nrow = N)
colnames(d_observed) <- paste0(c("x","y"),
                               rep(2:t, each = k))
d_observed <- as.data.frame(d_observed)
d_wide <- cbind(d_wide, d_observed)


for (i in 2:t){
  d_wide[, paste0("x", i)] <-
    c_x_x * d_wide[, paste0("x", (i-1))] +
    (rep(c_x_y, N) + c_x_y_z1 * d_wide$z1 + c_x_y_z2 * d_wide$z2 + d_wide$etaxy) * d_wide[, paste0("y", (i-1))] +
    c_x_z1 * d_wide$z1 +
    c_x_z2 * d_wide$z2 +
    1 * d_wide$etax +
    1 * d_wide[, paste0("eps_x", i)]
  
  d_wide[, paste0("y",i)] <-
    (rep(c_y_x, N) + c_y_x_z2 * d_wide$z2 + c_y_x_z3 * d_wide$z3 + d_wide$etayx) * d_wide[, paste0("x", (i-1))] +
    c_y_y * d_wide[, paste0("y", (i-1))] +
    c_y_z2 * d_wide$z2 +
    c_y_z3 * d_wide$z3 +
    1 * d_wide$etay +
    1 * d_wide[, paste0("eps_y", i)]
}

# add TRUE person-specific intercepts and TRUE person-specific cross-lagged
# coefficients to data set
d_wide$gamma_xy <- rep(c_x_y, N) +
  d_wide$etaxy + 
  c_x_y_z1 * d_wide$z1 + 
  c_x_y_z2 * d_wide$z2

d_wide$gamma_yx <- rep(c_y_x, N) + 
  d_wide$etayx + 
  c_y_x_z2 * d_wide$z2 + 
  c_y_x_z3 * d_wide$z3

d_wide$mu_x <- d_wide$etax + 
  c_x_z1 * d_wide$z1 + 
  c_x_z2 * d_wide$z2

d_wide$mu_y <- d_wide$etay + 
  c_y_z2 * d_wide$z2 + 
  c_y_z3 * d_wide$z3

###########################
#
# PART II Run fit_panel_sem() function with the nonlinear nonadditive model
# specification and store the panelSEM object using the label internal_list
#
###########################

## create variable names
time_varying_variables_standard_T6 <- list(c("x1", "x2", "x3", "x4", "x5", "x6"),
                                           c("y1", "y2", "y3", "y4", "y5", "y6"))

time_invariant_variables_standard <- list(c("z1","z2"),
                                          c("z2","z3"))


data <- d_wide
time_varying_variables <- time_varying_variables_standard_T6
time_invariant_variables <- time_invariant_variables_standard
linear <- FALSE
heterogeneity <- c("additive","cross-lagged")
use_open_mx <- TRUE
verbose <- 2
use_resamples <- FALSE

panelSEM_object <- fit_panel_sem(data = data,
                                 time_varying_variables = time_varying_variables,
                                 time_invariant_variables = time_invariant_variables,
                                 linear = linear,
                                 heterogeneity = heterogeneity,
                                 use_resamples = use_resamples,
                                 use_open_mx = use_open_mx,
                                 verbose = verbose)
internal_list <- panelSEM_object

##############################################
#
# PART III: Starting Values computation 
#
##############################################
n_occasions <- internal_list$info_model$n_occasions

n_processes <- internal_list$info_model$n_processes

labels_time_varying_variables <-
  as.vector(t(internal_list$info_variables$user_names_time_varying))

labels_time_invariant_variables <-
  internal_list$info_variables$info_time_invariant_variables

process_names <-
  internal_list$info_variables$names_processes["user_names",]

linear <- internal_list$info_model$linear
heterogeneity <- internal_list$info_model$heterogeneity
verbose <- internal_list$control$verbose
use_open_mx <- internal_list$info_model$use_open_mx

# Create Dataframe of Observed Variables Only
d_wide_observed <- d_wide[, c(names_time_invariant,
                              names_time_varying)]

# create a separate data frame for each person
person_data <- function(x){
  
  d_person <- matrix(as.numeric(x[labels_time_varying_variables]),
                     ncol = n_processes,
                     nrow = n_occasions)
  d_person <- as.data.frame(d_person)
  colnames(d_person) <- process_names
  return(d_person)
  
}

list_person_data <-
  apply(d_wide_observed,
        1,
        person_data)

# estimate vector-autoregressive model of order 1 for each person
library(vars)
fit_VAR_list <- lapply(list_person_data, FUN = VAR, p = 1, type = "const")

# exctract the person specific parameter estimates from the VAR-output
# and store them in a data.frame called matrix_person_coefficients
extract_parameters <- function(x){
  extract_coef <- numeric(0)
  for (i in 1:n_processes){
    for (j in 1:n_processes){
      extract_step <- coef(x)[[i]][paste0(process_names[j],".l1"),"Estimate"]
      extract_coef <-c(extract_coef, extract_step)
    }
  }
  
  extract_intercept <- numeric(0)
  for (i in 1:n_processes){
    extract_step <- coef(x)[[i]]["const","Estimate"]
    extract_intercept <-c(extract_intercept, extract_step)
  }
  
  extract_coef_se <- numeric(0)
  for (i in 1:n_processes){
    for (j in 1:n_processes){
      extract_step_se <- coef(x)[[i]][paste0(process_names[j],".l1"),"Std. Error"]
      extract_coef_se <-c(extract_coef_se, extract_step_se)
    }
  }
  
  extract_intercept_se <- numeric(0)
  for (i in 1:n_processes){
    extract_step_se <- coef(x)[[i]]["const","Std. Error"]
    extract_intercept_se <-c(extract_intercept_se, extract_step_se)
  }
  
  gamma <-
    matrix(ncol = internal_list$info_model$n_processes,
           nrow = internal_list$info_model$n_processes)
  
  rownames(gamma) <- colnames(gamma) <- process_names
  
  gamma[,] <-
    paste0("c",
           apply(expand.grid(rownames(gamma), colnames(gamma)),
                 1,
                 paste,
                 collapse = ""))
  
  names_extract <-
    c(paste0(t(gamma),"_hat"),
      paste0("ri_",internal_list$info_variables$names_time_invariant_unobserved_additive["user_names",],"_hat"),
      paste0(t(gamma),"_se"),
      paste0("ri_",internal_list$info_variables$names_time_invariant_unobserved_additive["user_names",],
             "_se"))
  
  person_specific_values <- matrix(ncol = length(names_extract))
  colnames(person_specific_values) <- names_extract
  
  person_specific_values[1,] <- c(extract_coef,
                                  extract_intercept,
                                  extract_coef_se,
                                  extract_intercept_se)
  return(person_specific_values)
}

list_person_coefficients <-
  lapply(fit_VAR_list, FUN = extract_parameters)


matrix_person_coefficients <-
  matrix(ncol = length(list_person_coefficients[[1]]),
         nrow = length(list_person_coefficients),
         unlist(list_person_coefficients),
         byrow = TRUE)

colnames(matrix_person_coefficients) <- colnames(list_person_coefficients[[1]])
matrix_person_coefficients <- as.data.frame(matrix_person_coefficients)

# merge data_wide with person-specific values of coefficients
d_wide <- cbind(d_wide, matrix_person_coefficients)

############################################
#
# estimate the eta-terms and check the models used to do so by also running them
# with the TRUE eta variables from the data generation
#
############################################
####### additive heterogeneity etax and etay
### etax
reg_etax <- lm(ri_etax_hat ~ z1 + z2, data = d_wide)
summary(reg_etax)

reg_mu_x <- lm(mu_x ~ z1 + z2, data = d_wide)
summary(reg_mu_x)

# add starting values to population value table
population_parameters[c("c_x_z1","c_x_z2"),]
population_parameters["c_x_z1","start_1"] <- coef(reg_etax)["z1"]
population_parameters["c_x_z2","start_1"] <- coef(reg_etax)["z2"]

# how strong do the residuals correlate with the true eta_x terms?
cor(resid(reg_mu_x), d_wide$etax)
cor(resid(reg_etax), d_wide$etax)

# how good does the (squared) residual standard error approximate the 
# variance of eta_x
summary(reg_mu_x)$sigma^2
summary(reg_etax)$sigma^2

population_parameters["psi_etax_etax","start_1"] <- summary(reg_etax)$sigma^2
# correct the variance for the estimation error by substracting the mean
# estimation error
population_parameters["psi_etax_etax","start_2"] <- 
   (summary(reg_etax)$sigma^2 - mean(d_wide$ri_etax_se^2))

### etay
reg_etay <- lm(ri_etay_hat ~ z2 + z3, data = d_wide)
summary(reg_etay)

reg_mu_y <- lm(mu_y ~ z2 + z3, data = d_wide)
summary(reg_mu_y)

# add starting values to population value table
population_parameters["c_y_z2","start_1"] <- coef(reg_etay)["z2"]
population_parameters["c_y_z3","start_1"] <- coef(reg_etay)["z3"]

# how good does the (squared) residual standard error approximate the 
# variance of eta_x
summary(reg_mu_y)$sigma^2
summary(reg_etay)$sigma^2

population_parameters["psi_etay_etay","start_1"] <- summary(reg_etay)$sigma^2
# correct the variance for the estimation error by substracting the mean
# estimation error
population_parameters["psi_etay_etay","start_2"] <- 
  (summary(reg_etay)$sigma^2 - mean(d_wide$ri_etay_se^2))

# how strong do the residuals correlate with the eta_y terms?
cor(resid(reg_mu_y), d_wide$etay)
cor(resid(reg_etay), d_wide$etay)

####### nonadditive heterogeneity etaxy and etayx
### etaxy 
reg_cxy <- lm(cxy_hat ~ z1 + z2, data = d_wide)
summary(reg_cxy)

reg_gamma_xy <- lm(gamma_xy ~ z1 + z2, data = d_wide)
summary(reg_gamma_xy)

# add starting values to table with true population values
population_parameters["c_x_y_z1","start_1"] <- coef(reg_cxy)["z1"]
population_parameters["c_x_y_z2","start_1"] <- coef(reg_cxy)["z2"]

# how good does the (squared) residual standard error approximate the 
# variance of eta_xy
summary(reg_gamma_xy)$sigma^2
summary(reg_cxy)$sigma^2

population_parameters["psi_etaxy_etaxy","start_1"] <- summary(reg_cxy)$sigma^2
# correct the variance for the estimation error by substracting the mean
# estimation error
population_parameters["psi_etaxy_etaxy","start_2"] <- 
  (summary(reg_cxy)$sigma^2 - mean(d_wide$cxy_se^2))

# how strong do the residuals correlate with the eta_xy terms?
cor(resid(reg_gamma_xy), d_wide$etaxy)
cor(resid(reg_cxy), d_wide$etaxy)

### etayx 
reg_cyx <- lm(cyx_hat ~ z2 + z3, data = d_wide)
summary(reg_cyx)

reg_gamma_yx <- lm(gamma_yx ~ z2 + z3, data = d_wide)
summary(reg_gamma_yx)

# add starting values to the table with the true population values
population_parameters["c_y_x_z2","start_1"] <- coef(reg_cyx)["z2"]
population_parameters["c_y_x_z3","start_1"] <- coef(reg_cyx)["z3"]

# how good does the (squared) residual standard error approximate the 
# variance of eta_yx
summary(reg_gamma_yx)$sigma^2
summary(reg_cyx)$sigma^2

population_parameters["psi_etayx_etayx","start_1"] <- summary(reg_cyx)$sigma^2
# correct the variance for the estimation error by substracting the mean
# estimation error
population_parameters["psi_etayx_etayx","start_2"] <- 
  (summary(reg_cyx)$sigma^2 - mean(d_wide$cxy_se^2))

# how strong do the residuals correlate with the eta_xy terms?
cor(resid(reg_gamma_yx), d_wide$etayx)
cor(resid(reg_cyx), d_wide$etayx)

# create a data frame with the residuals from the above regressions
# which will be used as PROXIES the unobserved time-invariant variables
names_unobserved <-
  c(internal_list$info_variables$names_time_invariant_unobserved_additive["user_names",],
    internal_list$info_variables$names_time_invariant_unobserved_cross_lagged["user_names",])

eta_hat <- matrix(ncol = length(names_unobserved), 
                  nrow = nrow(d_wide))
eta_hat[,1] <-resid(reg_etax) 
eta_hat[,2] <-resid(reg_etay) 
eta_hat[,3] <-resid(reg_cxy) 
eta_hat[,4] <-resid(reg_cyx) 
colnames(eta_hat) <- paste0(names_unobserved,"_hat")
d_wide <- cbind(d_wide, eta_hat)

# compute starting values for the (co-)variances of the eta-variables
match_names <- vector("character")
covariance_value <- vector("numeric")
for (i in gsub("_hat","",colnames(eta_hat))){
  for (j in gsub("_hat","",colnames(eta_hat))){
    match_names <- c(match_names,paste0("psi_",i,"_",j))
    covariance_value <- 
      c(covariance_value,
        cov(eta_hat)[paste0(i,"_hat"),paste0(j,"_hat")])
  }
}

matched_covariances <- data.frame(names = match_names,
                                  covariance_value = covariance_value)

for (i in 1 : nrow(matched_covariances)){
index_param <- which(rownames(population_parameters) == match_names[i])
population_parameters[index_param, "start_1"] <- 
  matched_covariances[i, "covariance_value"]
}

# add the product terms of the nonlinear part to the data
## product terms of time-varying variables and z-variables
product_terms_names <- character(0)

for (i in 1:internal_list$info_model$n_processes){
  j <- (internal_list$info_model$n_processes - i + 1)
  names_product <-
    as.vector(outer(
      internal_list$info_variables$info_time_invariant_variables[[j]],
      internal_list$info_variables$user_names_time_varying[
        i,
        -internal_list$info_model$n_occasions],
      paste,
      sep="_"))
  
  product_terms_names <- c(product_terms_names, names_product)
}

product_terms_names <- paste0("prod_",product_terms_names)
product_final <- numeric(0)

for (i in 1:internal_list$info_model$n_processes){
  k <- (internal_list$info_model$n_processes - i + 1)
  mat <- internal_list$info_variables$info_time_invariant_variables[[k]]
  vec <- internal_list$info_variables$user_names_time_varying[
    i,
    -internal_list$info_model$n_occasions]
  
  for (j in 1:(internal_list$info_model$n_occasions - 1)){
    product <- sweep(as.matrix(data[, mat]),
                     MARGIN=1,
                     as.numeric(data[, vec[j]]),
                     `*`)
    
    product_final <- cbind(product_final, product)
  }
}

data_product_terms <- as.data.frame(product_final)
colnames(data_product_terms) <- product_terms_names
d_wide <- cbind(d_wide, data_product_terms)

## product terms of time-varying variables and the PROXIES of the eta-variables
product_terms_names_eta <- character(0)

for (i in 1:internal_list$info_model$n_processes){
  k <- (internal_list$info_model$n_processes - i + 1)
  names_product <-
    as.vector(outer(
      internal_list$info_variables$names_time_invariant_unobserved_cross_lagged["user_names",k],
      internal_list$info_variables$user_names_time_varying[
        i,
        -internal_list$info_model$n_occasions],
      paste,
      sep="_"))
  
  product_terms_names_eta <- c(product_terms_names_eta, names_product)
}

product_terms_names_eta <- paste0("prod_", product_terms_names_eta,"_hat")
product_final_eta <- numeric(0)

for (i in 1:internal_list$info_model$n_processes){
  k <- (internal_list$info_model$n_processes - i + 1)
  mat <- internal_list$info_variables$names_time_invariant_unobserved_cross_lagged["user_names",k]
  vec <- internal_list$info_variables$user_names_time_varying[
    i,
    -internal_list$info_model$n_occasions]
  
  for (j in 1:(internal_list$info_model$n_occasions - 1)){
    product <- sweep(as.matrix(d_wide[, mat]),
                     MARGIN=1,
                     as.numeric(d_wide[, vec[j]]),
                     `*`)
    
    product_final_eta <- cbind(product_final_eta, product)
  }
}

data_product_terms_eta <- as.data.frame(product_final_eta)
colnames(data_product_terms_eta) <- product_terms_names_eta
d_wide <- cbind(d_wide, data_product_terms_eta)

# set up the nonlinear nonadditive model using the product variables and the 
# PROXIES defined above
# TODO:
# 1) automatize the model setup for an arbitrary number of time points (and 
# ideally for an arbitrary number of processes, but this can be done in a 
# subsequent step)
# 2) restrict the variances of the eta-terms to the corrrected values in the
# column start_2 of the object population_parameters
# 3) play around with an iterative procedure in which parameters that are 
# close to the true values are fixed to those values in subsequent estimation
# steps

model_PROXY <- '
# initial variables from observed time-invariant
x1 ~ c_x1_z1 * z1
x1 ~ c_x1_z2 * z2
x1 ~ c_x1_z3 * z3

y1 ~ c_y1_z1 * z1
y1 ~ c_y1_z2 * z2
y1 ~ c_y1_z3 * z3

# initial variables from unobserved time-invariant
x1 ~ c_x1_etax * etax_hat
x1 ~ c_x1_etay * etay_hat
x1 ~ c_x1_etaxy * etaxy_hat
x1 ~ c_x1_etayx * etayx_hat

y1 ~ c_y1_etax * etax_hat
y1 ~ c_y1_etay * etay_hat
y1 ~ c_y1_etaxy * etaxy_hat
y1 ~ c_y1_etayx * etayx_hat

# time-varying non-initial from observed time-invariant
x2 ~ c_x_z1 * z1
x2 ~ c_x_z2 * z2
x3 ~ c_x_z1 * z1
x3 ~ c_x_z2 * z2
x4 ~ c_x_z1 * z1
x4 ~ c_x_z2 * z2
x5 ~ c_x_z1 * z1
x5 ~ c_x_z2 * z2
x6 ~ c_x_z1 * z1
x6 ~ c_x_z2 * z2

y2 ~ c_y_z2 * z2
y2 ~ c_y_z3 * z3
y3 ~ c_y_z2 * z2
y3 ~ c_y_z3 * z3
y4 ~ c_y_z2 * z2
y4 ~ c_y_z3 * z3
y5 ~ c_y_z2 * z2
y5 ~ c_y_z3 * z3
y6 ~ c_y_z2 * z2
y6 ~ c_y_z3 * z3

# time-varying non-initial from unobserved time-invariant (additive)
x2 ~ 1 * etax_hat
x3 ~ 1 * etax_hat
x4 ~ 1 * etax_hat
x5 ~ 1 * etax_hat
x6 ~ 1 * etax_hat

y2 ~ 1 * etay_hat
y3 ~ 1 * etay_hat
y4 ~ 1 * etay_hat
y5 ~ 1 * etay_hat
y6 ~ 1 * etay_hat

# time-varying non-initial from unobserved time-invariant (non-additive)
x2 ~ 1 * prod_etaxy_y1_hat
x3 ~ 1 * prod_etaxy_y2_hat
x4 ~ 1 * prod_etaxy_y3_hat
x5 ~ 1 * prod_etaxy_y4_hat
x6 ~ 1 * prod_etaxy_y5_hat

y2 ~ 1 * prod_etayx_x1_hat
y3 ~ 1 * prod_etayx_x2_hat
y4 ~ 1 * prod_etayx_x3_hat
y5 ~ 1 * prod_etayx_x4_hat
y6 ~ 1 * prod_etayx_x5_hat

# time-varying non-initial onto time-varying (linear) 
x2 ~ c_x_x * x1
x2 ~ c_x_y * y1
x3 ~ c_x_x * x2
x3 ~ c_x_y * y2
x4 ~ c_x_x * x3
x4 ~ c_x_y * y3
x5 ~ c_x_x * x4
x5 ~ c_x_y * y4
x6 ~ c_x_x * x5
x6 ~ c_x_y * y5

y2 ~ c_y_x * x1
y2 ~ c_y_y * y1
y3 ~ c_y_x * x2
y3 ~ c_y_y * y2
y4 ~ c_y_x * x3
y4 ~ c_y_y * y3
y5 ~ c_y_x * x4
y5 ~ c_y_y * y4
y6 ~ c_y_x * x5
y6 ~ c_y_y * y5

# time-varying non-initial onto time-varying (non-linear) 
x2 ~ c_x_y_z1 * prod_z1_y1
x2 ~ c_x_y_z2 * prod_z2_y1
x3 ~ c_x_y_z1 * prod_z1_y2
x3 ~ c_x_y_z2 * prod_z2_y2
x4 ~ c_x_y_z1 * prod_z1_y3
x4 ~ c_x_y_z2 * prod_z2_y3
x5 ~ c_x_y_z1 * prod_z1_y4
x5 ~ c_x_y_z2 * prod_z2_y4
x6 ~ c_x_y_z1 * prod_z1_y5
x6 ~ c_x_y_z2 * prod_z2_y5

y2 ~ c_y_x_z2 * prod_z2_x1
y2 ~ c_y_x_z3 * prod_z3_x1
y3 ~ c_y_x_z2 * prod_z2_x2
y3 ~ c_y_x_z3 * prod_z3_x2
y4 ~ c_y_x_z2 * prod_z2_x3
y4 ~ c_y_x_z3 * prod_z3_x3
y5 ~ c_y_x_z2 * prod_z2_x4
y5 ~ c_y_x_z3 * prod_z3_x4
y6 ~ c_y_x_z2 * prod_z2_x5
y6 ~ c_y_x_z3 * prod_z3_x5

# covariances 
etax_hat ~~ psi_etax_etax * etax_hat
etax_hat ~~ psi_etax_etay * etay_hat
etax_hat ~~ psi_etax_etaxy * etaxy_hat
etax_hat ~~ psi_etax_etayx * etayx_hat
etay_hat ~~ psi_etay_etay * etay_hat
etay_hat ~~ psi_etay_etaxy * etaxy_hat
etay_hat ~~ psi_etay_etayx * etayx_hat
etaxy_hat ~~ psi_etaxy_etaxy * etaxy_hat
etaxy_hat ~~ psi_etaxy_etayx * etayx_hat
etayx_hat ~~ psi_etayx_etayx * etayx_hat

z1 ~~ psi_z1_z1 * z1
z1 ~~ psi_z1_z2 * z2
z1 ~~ psi_z1_z3 * z3
z2 ~~ psi_z2_z2 * z2
z2 ~~ psi_z2_z3 * z3
z3 ~~ psi_z3_z3 * z3

x1 ~~ psi_x1_x1 * x1
x1 ~~ psi_x1_y1 * y1
y1 ~~ psi_y1_y1 * y1

x2 ~~ psi_x_x * x2
y2 ~~ psi_y_y * y2
x3 ~~ psi_x_x * x3
y3 ~~ psi_y_y * y3
x4 ~~ psi_x_x * x4
y4 ~~ psi_y_y * y4
x5 ~~ psi_x_x * x5
y5 ~~ psi_y_y * y5
x6 ~~ psi_x_x * x6
y6 ~~ psi_y_y * y6
'

fit_model_PROXY <- lavaan::sem(model_PROXY, data = d_wide)
par_table_fit_model_PROXY <- parTable(fit_model_PROXY)


for(i in rownames(population_parameters)){
  population_parameters$start_3[rownames(population_parameters) == i] <-
    par_table_fit_model_PROXY$est[par_table_fit_model_PROXY$label == i]
}


