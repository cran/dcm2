test_that("test fisher_partial_p_alpha_l", {
  base_rates <- matrix(c(.4, .6), nrow = 1)
  colnames(base_rates) <- att_profile(ncol(data_att1$q_matrix))

  output <- fisher_partial_p_alpha_l(base_rates)

  expect_equal(typeof(output), "double")
  expect_equal(length(output), 1)
  expect_gte(output, 0)
  expect_lte(output, 1)
})

test_that("test calc_bivariate_prob", {
  base_rates <- matrix(c(.4, .6), nrow = 1)
  colnames(base_rates) <- att_profile(ncol(data_att1$q_matrix))

  num_items <- 2

  pi_matrix <- matrix(c(.3, .65,
                        .21, .76),
                      nrow = 2, ncol = 2, byrow = TRUE)

  bi <- matrix(NA, num_items, num_items)
  output <- calc_bivariate_prob(num_items, bi, pi_matrix, base_rates)

  expect_equal(length(output), num_items * num_items)
  expect_equal(typeof(output), "double")
})

test_that("test calc_univariate_prob", {
  base_rates <- matrix(c(.4, .6), nrow = 1)
  colnames(base_rates) <- att_profile(ncol(data_att1$q_matrix))

  num_items <- 2

  pi_matrix <- matrix(c(.3, .65,
                        .21, .76),
                      nrow = 2, ncol = 2, byrow = TRUE)
  uni <- numeric(num_items)

  output <- calc_univariate_prob(num_items, uni, pi_matrix, base_rates)

  expect_equal(length(output), num_items)
  expect_equal(typeof(output), "double")
})

test_that("test rmsea_calc", {
  # example from Liu et al 2016
  x2 <- 68.369
  df <- 28
  n <- 536

  rmsea <- rmsea_calc(x2, df, n)

  expect_equal(length(rmsea), 1)
  expect_equal(rmsea, 0.052, tolerance = .01)
})

test_that("test rmsea_ci", {
  # example from Liu et al 2016
  x2 <- 68.369
  df <- 28
  n <- 536
  ci_lower <- .05
  ci_upper <- .95

  ci <- rmsea_ci(x2, df, n, ci_lower, ci_upper)

  expect_equal(length(ci), 2)
  expect_equal(ci[1], 0.036, tolerance = .015)
  expect_equal(ci[2], 0.068, tolerance = .015)
})

test_that("test skills", {
  qmatrix <- tibble::tibble(att_1 = c(1, 0, 1, 0, 1, 0),
                            att_2 = c(0, 1, 0, 1, 0, 1))

  base_rates <- matrix(c(.2, .4, .3, .1), nrow = 1)
  colnames(base_rates) <- att_profile(ncol(qmatrix))

  l <- 2 ^ ncol(qmatrix)

  skills_output <- skills(base_rates, l, qmatrix)

  expected_output <- tibble::tibble(prof1 = c("10", "01", "10", "01", "10",
                                              "01"),
                                    prof2 = c("00", "01", "00", "01", "00",
                                              "01"),
                                    prof3 = c("10", "00", "10", "00", "10",
                                              "00"),
                                    prof4 = c("00", "00", "00", "00", "00",
                                              "00")) %>%
    as.matrix()

  expected_output <- unname(expected_output)

  expect_equal(ncol(skills_output), l)
  expect_equal(nrow(skills_output), nrow(qmatrix))
  expect_equal(skills_output, expected_output)
})

test_that("test calc_patt", {
  q_matrix <- tibble::tibble(att_1 = c(1, 0, 1, 0),
                             att_2 = c(0, 1, 0, 1))
  l <- 2^ncol(q_matrix)
  skills_missing <- tibble::tibble(`00` = c("10", "01", "10", "01"),
                                   `10` = c("00", "01", "00", "01"),
                                   `01` = c("10", "00", "10", "00"),
                                   `11` = c("00", "00", "00", "00")) %>%
    as.matrix()

  output <- calc_patt(q_matrix, l, skills_missing)

  expect_equal(typeof(output), "integer")
  expect_equal(ncol(output), l)
  expect_equal(nrow(output), nrow(q_matrix))
  expect_equal(output,
               tibble::tibble(`00` = rep(1, nrow(q_matrix)),
                              `10` = c(2, 1, 2, 1),
                              `01` = c(1, 2, 1, 2),
                              `11` = rep(2, nrow(q_matrix))) %>%
                 as.matrix() %>%
                 unname())
})

test_that("test item_param_profiles", {
  natt1 <- 1
  natt2 <- 2
  natt3 <- 3
  natt4 <- 4

  output1 <- item_param_profiles(natt1)
  output2 <- item_param_profiles(natt2)
  output3 <- item_param_profiles(natt3)
  output4 <- item_param_profiles(natt4)

  expect_equal(output1, c("Intercept", "MEF1"))
  expect_equal(output2, c("Intercept", "MEF1", "MEF2", "Int12"))
  expect_equal(output3, c("Intercept", "MEF1", "MEF2", "MEF3",
                          "Int12", "Int13", "Int23", "Int123"))
  expect_equal(output4, c("Intercept", "MEF1", "MEF2", "MEF3", "MEF4",
                          "Int12", "Int13", "Int14", "Int23", "Int24",
                          "Int34", "Int123", "Int124", "Int134",
                          "Int234", "Int1234"))
})

test_that("test att_profile", {
  natt <- 3

  expect_equal(length(att_profile(natt)), 2^natt)
  expect_equal(att_profile(natt), c("000", "100", "010", "001",
                                    "110", "101", "011", "111"))
})

test_that("test calc_design_matrix - LCDM", {
  qmatrix <- tibble::tibble(att_1 = c(1, 0, 1, 1),
                            att_2 = c(0, 1, 0, 1))
  num_item_params <- c(2, 2, 2, 4)
  model_type <- "LCDM"

  output <- calc_design_matrix(num_item_params, qmatrix, model_type)

  expect_equal(length(output), nrow(qmatrix))
  expect_equal(output[[1]],
               matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[2]],
               matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[3]],
               matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[4]],
               matrix(c(1, 1, 1, 1,
                        0, 1, 0, 1,
                        0, 0, 1, 1,
                        0, 0, 0, 1),
                      nrow = 4, ncol = 4, byrow = FALSE))
})

test_that("test calc_design_matrix - DINO", {
  qmatrix <- tibble::tibble(att_1 = c(1, 0, 1, 1),
                            att_2 = c(0, 1, 0, 1))
  num_item_params <- c(2, 2, 2, 2)
  model_type <- "DINO"

  output <- calc_design_matrix(num_item_params, qmatrix, model_type)

  expect_equal(length(output), nrow(qmatrix))
  expect_equal(output[[1]],
               matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[2]],
               matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[3]],
               matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[4]],
               matrix(c(1, 1, 1, 1,
                        0, 1, 1, 1),
                      nrow = 4, ncol = 2, byrow = FALSE))
})

test_that("test calc_design_matrix - DINA", {
  qmatrix <- tibble::tibble(att_1 = c(1, 0, 1, 1),
                            att_2 = c(0, 1, 0, 1))
  num_item_params <- c(2, 2, 2, 2)
  model_type <- "DINA"

  output <- calc_design_matrix(num_item_params, qmatrix, model_type)

  expect_equal(length(output), nrow(qmatrix))
  expect_equal(output[[1]],
               matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[2]],
               matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[3]],
               matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[4]],
               matrix(c(1, 1, 1, 1,
                        0, 0, 0, 1),
                      nrow = 4, ncol = 2, byrow = FALSE))
})

test_that("test calc_design_matrix - ACDM", {
  qmatrix <- tibble::tibble(att_1 = c(1, 0, 1, 1),
                            att_2 = c(0, 1, 0, 1))
  num_item_params <- c(2, 2, 2, 3)
  model_type <- "ACDM"

  output <- calc_design_matrix(num_item_params, qmatrix, model_type)

  expect_equal(length(output), nrow(qmatrix))
  expect_equal(output[[1]],
               matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[2]],
               matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[3]],
               matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[4]],
               matrix(c(1, 1, 1, 1,
                        0, 1, 0, 1,
                        0, 0, 1, 1),
                      nrow = 4, ncol = 3, byrow = FALSE))
})

test_that("test calc_design_matrix - BUGDINO", {
  qmatrix <- tibble::tibble(att_1 = c(1, 0, 1, 1),
                            att_2 = c(0, 1, 0, 1))
  num_item_params <- c(2, 2, 2, 2)
  model_type <- "BUGDINO"

  output <- calc_design_matrix(num_item_params, qmatrix, model_type)

  expect_equal(length(output), nrow(qmatrix))
  expect_equal(output[[1]],
               matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[2]],
               matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[3]],
               matrix(c(1, 1, 1, 0), nrow = 2, ncol = 2, byrow = FALSE))
  expect_equal(output[[4]],
               matrix(c(1, 1, 1, 1,
                        1, 0, 0, 0),
                      nrow = 4, ncol = 2, byrow = FALSE))
})

test_that("test possible_parameters - LCDM", {
  output <- possible_parameters(2, "LCDM")

  expect_equal(output,
               matrix(c(0, 0,
                        1, 0,
                        0, 1,
                        1, 1),
                      nrow = 4, ncol = 2, byrow = TRUE))
})

test_that("test possible_parameters - ACDM", {
  output <- possible_parameters(2, "ACDM")

  expect_equal(output,
               matrix(c(0, 0,
                        1, 0,
                        0, 1,
                        1, 1),
                      nrow = 4, ncol = 2, byrow = TRUE))
})

test_that("test possible_parameters - LLM", {
  output <- possible_parameters(2, "LLM")

  expect_equal(output,
               matrix(c(0, 0,
                        1, 0,
                        0, 1,
                        1, 1),
                      nrow = 4, ncol = 2, byrow = TRUE))
})

test_that("test possible_parameters - RRUM", {
  output <- possible_parameters(2, "RRUM")

  expect_equal(output,
               matrix(c(0, 0,
                        1, 0,
                        0, 1,
                        1, 1),
                      nrow = 4, ncol = 2, byrow = TRUE))
})

test_that("test possible_parameters - DINO", {
  output <- possible_parameters(2, "DINO")

  expect_equal(output,
               matrix(c(1, 0,
                        1, 1,
                        1, 1,
                        1, 1),
                      nrow = 4, ncol = 2, byrow = TRUE))
})

test_that("test possible_parameters - DINA", {
  output <- possible_parameters(2, "DINA")

  expect_equal(output,
               matrix(c(1, 0,
                        1, 0,
                        1, 0,
                        1, 1),
                      nrow = 4, ncol = 2, byrow = TRUE))
})

test_that("test possible_parameters - BUGDINO", {
  output <- possible_parameters(2, "BUGDINO")

  expect_equal(output,
               matrix(c(1, 1,
                        1, 0,
                        1, 0,
                        1, 0),
                      nrow = 4, ncol = 2, byrow = TRUE))
})

test_that("test calc_emp_marginal_prob", {
  data <- matrix(data = c(1, 1,
                          1, 0,
                          0, 1,
                          1, 1,
                          0, 0),
                 nrow = 5, ncol = 2, byrow = TRUE)

  output <- calc_emp_marginal_prob(data, 5)

  expect_equal(output,
               c(.6, .6, .4))
})

test_that("test calc_mod_marginal_prob", {
  pi_matrix <- matrix(c(.3, .8,
                        .2, .7,
                        .15, .77,
                        .24, .90,
                        .30, .65),
                      nrow = 5, ncol = 2, byrow = TRUE)

  base_rates <- c(.5, .5)

  uni <- numeric(5)
  uni <- calc_univariate_prob(5, uni, pi_matrix, base_rates)

  bi <- matrix(NA, 5, 5)
  bi <- calc_bivariate_prob(5, bi, pi_matrix, base_rates)

  output <- calc_mod_marginal_prob(5, pi_matrix, base_rates)

  expect_equal(output,
               c(uni, bi[lower.tri(bi)]))
})

test_that("test calc_covariance_matrix", {
  pi_matrix <- matrix(c(.3, .8,
                        .2, .7,
                        .15, .77,
                        .24, .90,
                        .30, .65),
                      nrow = 5, ncol = 2, byrow = TRUE)

  base_rates <- c(.5, .5)

  partitioned_cov_mat <- Mord(c(1:5), pi_matrix, base_rates)

  output <- calc_covariance_matrix(5, pi_matrix, base_rates)

  expect_equal(output,
               cbind(rbind(partitioned_cov_mat$Xi11,
                           partitioned_cov_mat$Xi21),
                     rbind(t(partitioned_cov_mat$Xi21),
                           partitioned_cov_mat$Xi22)))
})

test_that("test calc_jacobian_matrix - logit link", {
  pi_matrix <- matrix(c(.3, .8,
                        .2, .7),
                      nrow = 2, ncol = 2, byrow = TRUE)

  base_rates <- matrix(c(.5, .5), nrow = 1, byrow = TRUE)
  colnames(base_rates) <- c("0", "1")

  num_item_params <- c(rep(2, 2))

  qmatrix <- tibble::tibble(att_1 = rep(1, 2))

  design_matrix <- calc_design_matrix(num_item_params, qmatrix, "LCDM")

  skills_missing <- skills(base_rates, 2, qmatrix)

  patt <- calc_patt(qmatrix, 2, skills_missing)

  link <- "logit"

  output <- calc_jacobian_matrix(2, num_item_params, pi_matrix,
                                 design_matrix, patt, base_rates, 2, 1, link)

  expect_equal(output,
               matrix(c(.185, .08, 0, 0, .075,
                        0, 0, .185, .105, .05,
                        .077, .056, .108, .084, .015),
                      nrow = 3, ncol = 5, byrow = TRUE))
})

test_that("test calc_jacobian_matrix - log link", {
  pi_matrix <- matrix(c(.3, .8,
                        .2, .7),
                      nrow = 2, ncol = 2, byrow = TRUE)

  base_rates <- matrix(c(.5, .5), nrow = 1, byrow = TRUE)
  colnames(base_rates) <- c("0", "1")

  num_item_params <- c(rep(2, 2))

  qmatrix <- tibble::tibble(att_1 = rep(1, 2))

  design_matrix <- calc_design_matrix(num_item_params, qmatrix, "LCDM")

  skills_missing <- skills(base_rates, 2, qmatrix)

  patt <- calc_patt(qmatrix, 2, skills_missing)

  link <- "log"

  output <- calc_jacobian_matrix(2, num_item_params, pi_matrix,
                                 design_matrix, patt, base_rates, 2, 1, link)

  expect_equal(output,
               matrix(c(.55, .4, 0, 0, .075,
                        0, 0, .45, .35, .05,
                        .31, .28, .31, .28, .015),
                      nrow = 3, ncol = 5, byrow = TRUE))
})

test_that("test calc_jacobian_matrix - identity link", {
  pi_matrix <- matrix(c(.3, .8,
                        .2, .7),
                      nrow = 2, ncol = 2, byrow = TRUE)

  base_rates <- matrix(c(.5, .5), nrow = 1, byrow = TRUE)
  colnames(base_rates) <- c("0", "1")

  num_item_params <- c(rep(2, 2))

  qmatrix <- tibble::tibble(att_1 = rep(1, 2))

  design_matrix <- calc_design_matrix(num_item_params, qmatrix, "LCDM")

  skills_missing <- skills(base_rates, 2, qmatrix)

  patt <- calc_patt(qmatrix, 2, skills_missing)

  link <- "identity"

  output <- calc_jacobian_matrix(2, num_item_params, pi_matrix,
                                 design_matrix, patt, base_rates, 2, 1, link)

  expect_equal(output,
               matrix(c(1, .5, 0, 0, .075,
                        0, 0, 1, .5, .05,
                        .45, .35, .55, .4, .015),
                      nrow = 3, ncol = 5, byrow = TRUE))
})

test_that("test calc_c_r", {
  num_items <- 5
  num_item_params <- c(2, 2, 2, 2, 2)
  pi_matrix <- matrix(c(.3, .7,
                        .25, .65,
                        .3, .85,
                        .15, .8,
                        .1, .7),
                      nrow = 5, ncol = 2, byrow = TRUE)
  base_rates <- matrix(c(.4, .6), nrow = 1)
  colnames(base_rates) <- c("0", "1")
  l <- 2
  num_attr <- 1
  qmatrix <- tibble::tibble(att_1 = c(1, 1, 1, 1, 1))
  model_type <- "LCDM"
  link <- "logit"

  output <- calc_c_r(num_items, num_item_params, pi_matrix, base_rates, l,
                     num_attr, qmatrix, model_type, link)

  expect_equal(typeof(output), "double")
  expect_equal(class(output), c("matrix", "array"))
  expect_equal(nrow(output), ncol(output))
  expect_equal(nrow(output), 15)
})
