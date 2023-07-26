test_that("create lavaan syntax", {
  library(panelSEM)
  library(testthat)
  set.seed(23)

  time_points <- 10

  ############################################
  # set values of population parameters SET 1
  ############################################

  # covariance-matrix of the latent traits
  A_sigma_eta <- matrix(nrow = 2, ncol = 2, c(1, 0.5, 0.5, 1))
  sigma_epsilon_eta <- t(A_sigma_eta) %*% A_sigma_eta

  # covariance-matrix of the epsilon_z variables
  A_sigma_z <- matrix(nrow = 3, ncol = 3, c(1, 0.75, 0.3, 0.75, 1, 0.25,
                                            0.3, 0.25, 1))
  sigma_epsilon_z <- t(A_sigma_z) %*% A_sigma_z

  # covariance matrix of the initial residuals
  A_sigma_eps_init <- matrix(nrow = 2, ncol = 2, c(1, 2, 2, 1))
  sigma_epsilon_init <- t(A_sigma_eps_init) %*% A_sigma_eps_init

  population_parameters <- data.frame(
    N = NA,
    # directed effects
    ## latent traits
    c_x1_etax = -4,
    c_x1_etay = 8,

    c_y1_etax = -12,
    c_y1_etay = 19,

    c_x_etax = 1,
    c_y_etay = 1,

    ## time independent predictors
    c_x1_z1 = -5,
    c_x1_z2 = -1,
    c_x1_z3 = 4,

    c_y1_z1 = - 8,
    c_y1_z2 = 2,
    c_y1_z3 = 6,

    c_x_z1 = 0.5,
    c_x_z2 = 2,

    c_y_z2 = 1.5,
    c_y_z3 = 2,

    ## autoregressive and cross-lagged effects
    c_x_x = 0.05,
    c_x_y = 0.4,
    c_y_x = -0.6,
    c_y_y = 1.2,

    # undirected effects
    ## trait
    psi_etax_etax = sigma_epsilon_eta[1,1],
    psi_etax_etay = sigma_epsilon_eta[1,2],
    psi_etay_etay = sigma_epsilon_eta[2,2],

    ## observed predictors
    psi_z1_z1 = sigma_epsilon_z[1,1],
    psi_z1_z2 = sigma_epsilon_z[1,2],
    psi_z1_z3 = sigma_epsilon_z[1,3],
    psi_z2_z2 = sigma_epsilon_z[2,2],
    psi_z2_z3 = sigma_epsilon_z[2,3],
    psi_z3_z3 = sigma_epsilon_z[3,3],

    ## residuals
    ### initial time point
    psi_x1_x1 = sigma_epsilon_init[1,1],
    psi_x1_y1 = sigma_epsilon_init[1,2],
    psi_y1_y1 = sigma_epsilon_init[2,2],

    ### subsequent time points
    psi_x_x = 1,
    psi_y_y = 1
  )

  population_parameters$N <- 1000

  population_parameters$time_points <- time_points

  data <- do.call(what = simulate_data,
                  args = population_parameters)


  ################
  #### LAVAAN ####
  ################

  # general settings
  time_varying_variables = list(paste0("x", 1:time_points),
                                paste0("y", 1:time_points))
  time_invariant_variables = list(c("z1", "z2"),
                                  c("z2", "z3"))
  use_open_mx = FALSE
  heterogeneity = "additive"

  linear = TRUE

  # set up internal list

  # create empty list
  internal_list <- panelSEM:::create_empty_list(verbose = 0)

  # assign class causalSEM to internal list
  internal_list <- panelSEM:::create_panelSEM_s3_object(internal_list = internal_list)

  # fill in user-specified data to the list
  internal_list <- panelSEM:::fill_in_data( internal_list = internal_list,
                                            data = data ,
                                            add_product_variables = FALSE)

  # fill in user-specified information about the model into the list
  internal_list <-
    panelSEM:::fill_in_info_variables(internal_list = internal_list,
                                      time_varying_variables = time_varying_variables,
                                      time_invariant_variables = time_invariant_variables,
                                      linear = linear,
                                      heterogeneity  = heterogeneity,
                                      use_open_mx = use_open_mx)

  # add product terms of observed variables if model is nonlinear
  if (linear == FALSE){
    internal_list <- fill_in_data(data = internal_list$info_data$data,
                                  internal_list = internal_list,
                                  add_product_variables = TRUE )}

  # fill in user-specified information about the model into the list
  internal_list <-
    fill_in_info_model(internal_list = internal_list,
                       time_varying_variables = time_varying_variables,
                       time_invariant_variables = time_invariant_variables,
                       linear = linear,
                       heterogeneity  = heterogeneity,
                       use_open_mx = use_open_mx)


  model <- panelSEM:::fill_in_model_specification(internal_list = internal_list)

  testthat::expect_true(is(model, "panelSEM"))
  testthat::expect_true(is(model$model_syntax$lavaan, "character"))
  testthat::expect_true(length(model$model_syntax$OpenMx) == 0)

  raw_syntax <- gsub(pattern = "-- Syntax generated with function version [0-9]_[0-9]_[0-9] 20[0-9][0-9]_[0-9][0-9]_[0-9][0-9] --",
                     replacement = "",
                     x = model$model_syntax$lavaan)

  expected_output <- "# panelSEM\n#\nx1 ~ c_x1_z1 * z1\nx1 ~ c_x1_z2 * z2\nx1 ~ c_x1_z3 * z3\ny1 ~ c_y1_z1 * z1\ny1 ~ c_y1_z2 * z2\ny1 ~ c_y1_z3 * z3\nx2 ~ c_x_z1 * z1\nx2 ~ c_x_z2 * z2\nx3 ~ c_x_z1 * z1\nx3 ~ c_x_z2 * z2\nx4 ~ c_x_z1 * z1\nx4 ~ c_x_z2 * z2\nx5 ~ c_x_z1 * z1\nx5 ~ c_x_z2 * z2\nx6 ~ c_x_z1 * z1\nx6 ~ c_x_z2 * z2\nx7 ~ c_x_z1 * z1\nx7 ~ c_x_z2 * z2\nx8 ~ c_x_z1 * z1\nx8 ~ c_x_z2 * z2\nx9 ~ c_x_z1 * z1\nx9 ~ c_x_z2 * z2\nx10 ~ c_x_z1 * z1\nx10 ~ c_x_z2 * z2\ny2 ~ c_y_z2 * z2\ny2 ~ c_y_z3 * z3\ny3 ~ c_y_z2 * z2\ny3 ~ c_y_z3 * z3\ny4 ~ c_y_z2 * z2\ny4 ~ c_y_z3 * z3\ny5 ~ c_y_z2 * z2\ny5 ~ c_y_z3 * z3\ny6 ~ c_y_z2 * z2\ny6 ~ c_y_z3 * z3\ny7 ~ c_y_z2 * z2\ny7 ~ c_y_z3 * z3\ny8 ~ c_y_z2 * z2\ny8 ~ c_y_z3 * z3\ny9 ~ c_y_z2 * z2\ny9 ~ c_y_z3 * z3\ny10 ~ c_y_z2 * z2\ny10 ~ c_y_z3 * z3\nx2 ~ c_x_x * x1\nx2 ~ c_x_y * y1\ny2 ~ c_y_x * x1\ny2 ~ c_y_y * y1\nx3 ~ c_x_x * x2\nx3 ~ c_x_y * y2\ny3 ~ c_y_x * x2\ny3 ~ c_y_y * y2\nx4 ~ c_x_x * x3\nx4 ~ c_x_y * y3\ny4 ~ c_y_x * x3\ny4 ~ c_y_y * y3\nx5 ~ c_x_x * x4\nx5 ~ c_x_y * y4\ny5 ~ c_y_x * x4\ny5 ~ c_y_y * y4\nx6 ~ c_x_x * x5\nx6 ~ c_x_y * y5\ny6 ~ c_y_x * x5\ny6 ~ c_y_y * y5\nx7 ~ c_x_x * x6\nx7 ~ c_x_y * y6\ny7 ~ c_y_x * x6\ny7 ~ c_y_y * y6\nx8 ~ c_x_x * x7\nx8 ~ c_x_y * y7\ny8 ~ c_y_x * x7\ny8 ~ c_y_y * y7\nx9 ~ c_x_x * x8\nx9 ~ c_x_y * y8\ny9 ~ c_y_x * x8\ny9 ~ c_y_y * y8\nx10 ~ c_x_x * x9\nx10 ~ c_x_y * y9\ny10 ~ c_y_x * x9\ny10 ~ c_y_y * y9\netax =~ c_x1_etax * x1\netay =~ c_x1_etay * x1\netax =~ c_y1_etax * y1\netay =~ c_y1_etay * y1\netax =~ 1 * x2\netax =~ 1 * x3\netax =~ 1 * x4\netax =~ 1 * x5\netax =~ 1 * x6\netax =~ 1 * x7\netax =~ 1 * x8\netax =~ 1 * x9\netax =~ 1 * x10\netay =~ 1 * y2\netay =~ 1 * y3\netay =~ 1 * y4\netay =~ 1 * y5\netay =~ 1 * y6\netay =~ 1 * y7\netay =~ 1 * y8\netay =~ 1 * y9\netay =~ 1 * y10\netax ~~ psi_etax_etax * etax\netax ~~ psi_etax_etay * etay\netay ~~ psi_etay_etay * etay\nz1 ~~ psi_z1_z1 * z1\nz1 ~~ psi_z1_z2 * z2\nz1 ~~ psi_z1_z3 * z3\nz2 ~~ psi_z2_z2 * z2\nz2 ~~ psi_z2_z3 * z3\nz3 ~~ psi_z3_z3 * z3\nx1 ~~ psi_x1_x1 * x1\nx1 ~~ psi_x1_y1 * y1\ny1 ~~ psi_y1_y1 * y1\nx2 ~~ psi_x_x * x2\ny2 ~~ psi_y_y * y2\nx3 ~~ psi_x_x * x3\ny3 ~~ psi_y_y * y3\nx4 ~~ psi_x_x * x4\ny4 ~~ psi_y_y * y4\nx5 ~~ psi_x_x * x5\ny5 ~~ psi_y_y * y5\nx6 ~~ psi_x_x * x6\ny6 ~~ psi_y_y * y6\nx7 ~~ psi_x_x * x7\ny7 ~~ psi_y_y * y7\nx8 ~~ psi_x_x * x8\ny8 ~~ psi_y_y * y8\nx9 ~~ psi_x_x * x9\ny9 ~~ psi_y_y * y9\nx10 ~~ psi_x_x * x10\ny10 ~~ psi_y_y * y10"

  testthat::expect_true(raw_syntax == expected_output)

})
