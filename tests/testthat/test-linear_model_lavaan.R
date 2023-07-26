test_that("test linear model - lavaan", {
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

  ## LAVAAN

  model <- fit_panel_sem(data = data,
                         time_varying_variables = list(paste0("x", 1:time_points),
                                                       paste0("y", 1:time_points)),
                         time_invariant_variables = list(c("z1", "z2"),
                                                         c("z2", "z3")),
                         use_open_mx = FALSE,
                         heterogeneity = "additive")

  fit_lavaan <- lavaan::sem(model$model_syntax$lavaan,
                            data = model$info_data$data)

  est <- lavaan::coef(fit_lavaan)[unique(names(coef(fit_lavaan)))]

  shared_names <- names(population_parameters)[names(population_parameters) %in% names(est)]

  expect_equal(
    all(
      abs(
        (est[shared_names] - population_parameters[shared_names])/population_parameters[shared_names]
      ) < .3),
    TRUE)


  model <- fit_panel_sem(data = data,
                         time_varying_variables = list(paste0("x", 1:time_points),
                                                       paste0("y", 1:time_points)),
                         time_invariant_variables = list(c("z1", "z2"),
                                                         c("z2", "z3")),
                         use_open_mx = FALSE,
                         homogeneous = TRUE)

  fit_lavaan <- try(lavaan::sem(model$model_syntax$lavaan,
                                data = model$info_data$data))
  expect_true(is(fit_lavaan, "lavaan"))

})
