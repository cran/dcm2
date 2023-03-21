#' Calculate empirical marginal probabilities model fit
#'
#' Calculates the empirical first- and second-order item marginal probabilities.
#'
#' @param data A data frame containing the raw data, where there is one row per
#' respondent and one column per item
#' @param n An integer specifying the number of respondents in `data`
#'
#' @return `p` A vector containing the first- and second-order empirical item
#' marginal probabilities.
#'
#' @noRd
calc_emp_marginal_prob <- function(data, n) {
  # calculate first-order marginal probabilities (ie for single items)
  p1 <- colMeans(data, na.rm = TRUE)

  # convert the data frame to a matrix
  x <- as.matrix(data)

  # calculate the second-order marginal probabilities (ie for item pairs)
  p2 <- crossprod(x, x) / n

  # create a vector with the first- and second-order marginal probabilities
  p <- c(as.matrix(p1), p2[lower.tri(p2)])
  return(p)
}

#' Calculate all possible attribute mastery profiles
#'
#' @param natt The number of assessed attributes.
#'
#' @return `profile` A vector containing all possible attribute mastery
#' profiles.
#'
#' @noRd
att_profile <- function(natt) {
  att_names <- glue::glue("att_{1:natt}") # nolint

  # all possible combinations of attribute mastery
  profile <- as_binary(natt) %>%
    # specify attribute names
    tibble::as_tibble(.name_repair = ~att_names) %>%
    # create attribute mastery profile
    tidyr::unite(., col = "profile", sep = "", remove = FALSE, na.rm = TRUE) %>%
    # pull attribute mastery profile
    dplyr::pull(profile)

  return(profile)
}

#' Calculate model marginal probabilities model fit
#'
#' Calculates the model-based first- and second-order item marginal
#' probabilities.
#'
#' @param num_items An integer specifying the number of items
#' @param pi_matrix An item-by-class matrix containing the probability of a
#' correct response by members of each latent class
#' @param base_rates A single row matrix containing the structural parameters
#' indicating the model estimated base rates of mastery
#'
#' @return `e` A vector containing the first- and second-order model-based item
#' marginal probabilities.
#'
#' @noRd
calc_mod_marginal_prob <- function(num_items, pi_matrix, base_rates) {
  # create an empty vector for the first-order marginal probabilities
  uni <- numeric(num_items)
  # calculate the first-order marginal probabilities
  uni <- calc_univariate_prob(num_items, uni, pi_matrix, base_rates)

  # create an empty matrix for the second-order marginal probabilities
  bi <- matrix(NA, num_items, num_items)
  # calculate the second-order marginal probabilities
  bi <- calc_bivariate_prob(num_items, bi, pi_matrix, base_rates)
  # create a vector with the first- and second-order marginal probabilities
  e <- c(uni, bi[lower.tri(bi)])

  return(e)
}

#' Calculate the C_r matrix
#'
#' @param num_items An integer specifying the number of items
#' @param num_item_params A vector containing the number of estimated item
#' parameters for each of the items
#' @param pi_matrix An item-by-class matrix containing the probability of a
#' correct response by members of each latent class
#' @param base_rates A single row matrix containing the structural parameters
#' indicating the model estimated base rates of mastery
#' @param l An integer containing the number of latent classes
#' @param num_attr An integer containing the number of assessed attributes
#' @param qmatrix A data frame containing the Q-matrix
#' @param model_type A string containing the type of model (e.g., 'LCDM')
#' @param link A string containing the type of link function (e.g., 'logit')
#'
#' @return `cr` An R x (R - F) orthogonal complement to \delta_r (Browne, 1984).
#'
#' @references Browne, M. W. (1984). Asymptotically distribution-free methods
#'    for the analysis of covariance structures.
#'    *British Journal of Mathematical and Statistical Psychology, 37*, 62-83.
#'    doi:10.1111/j.2044-8317.1984.tb00789.x
#'
#' @noRd
calc_c_r <- function(num_items, num_item_params, pi_matrix, base_rates, l,
                     num_attr, qmatrix, model_type, link) {
  design_matrix <- calc_design_matrix(num_item_params, qmatrix, model_type)

  skills_missing <- skills(base_rates, l, qmatrix)

  patt <- calc_patt(qmatrix, l, skills_missing)

  jacobian <- calc_jacobian_matrix(num_items, num_item_params, pi_matrix,
                                   design_matrix, patt, base_rates, l, num_attr,
                                   link)

  jacobian <- qr.Q(qr(jacobian),
                   complete = TRUE)[, (ncol(jacobian) +
                                         1L):ncol(qr.Q(qr(jacobian),
                                                       complete = TRUE)),
                                    drop = FALSE]

  covariance_matrix <- calc_covariance_matrix(num_items, pi_matrix, base_rates)

  cr <- jacobian %*% solve(t(jacobian) %*% covariance_matrix %*% jacobian) %*%
    t(jacobian)

  return(cr)
}

#' Calculate the Jacobian matrix
#'
#' @param num_items An integer specifying the number of items
#' @param num_item_params A vector containing the number of estimated item
#' parameters for each of the items
#' @param pi_matrix An item-by-class matrix containing the probability of a
#' correct response by members of each latent class
#' @param design_matrix A matrix containing the design matrix
#' @param patt A matrix containing the pattern matrix
#' @param base_rates A single row matrix containing the structural parameters
#' indicating the model estimated base rates of mastery
#' @param l An integer containing the number of latent classes
#' @param num_attr An integer containing the number of assessed attributes
#' @param link A string containing the type of link function (e.g., 'logit')
#'
#' @return `jacobian` A matrix containing the Jacobian matrix.
#'
#' @noRd
calc_jacobian_matrix <- function(num_items, num_item_params, pi_matrix,
                                 design_matrix, patt, base_rates, l, num_attr,
                                 link) {
  jacobian11 <- matrix(0, nrow = num_items, ncol = sum(num_item_params))
  cumulative_parameters <- cumsum(num_item_params)

  for (ii in 1:num_items) {
    # the Jacobian matrix is calculated differently depending on the link
    # function; this describes the calculation for the logit link function
    # which is the most complicated
    #
    # multiply the prob of a correct response times prob of incorrect response
    # times design matrix times base rate this estimates the covariance of the
    # first-order marginal probabilities by using the design matrix, this
    # controls for parameters not estimated for certain latent classes
    if (link == "logit") {
      jacobian11[ii,
                 (cumulative_parameters[ii] -
                    num_item_params[ii] + 1):cumulative_parameters[ii]] <-
        colSums(pi_matrix[ii, ] * (1 - pi_matrix[ii, ]) *
                  design_matrix[[ii]][patt[ii, ], ] * as.vector(base_rates))
    } else if (link == "log") {
      jacobian11[ii,
                 (cumulative_parameters[ii] -
                    num_item_params[ii] + 1):cumulative_parameters[ii]] <-
        colSums(pi_matrix[ii, ] *
                  design_matrix[[ii]][patt[ii, ], ] * as.vector(base_rates))
    } else if (link == "identity") {
      jacobian11[ii,
                 (cumulative_parameters[ii] -
                    num_item_params[ii] + 1):cumulative_parameters[ii]] <-
        colSums(design_matrix[[ii]][patt[ii, ], ] * as.vector(base_rates))
    }

  }

  # for jacobian12, we're calculating the difference in the prob of a correct
  # response between masters of all the attributes and the remaining latent
  # classes for a single attribute assessment, jacobian12 is just the prob of a
  # correct response from nonmasters
  if (num_attr == 1) {
    jacobian12 <- matrix(pi_matrix[, 1], ncol = 1)
  } else {
    jacobian12 <- pi_matrix %*% rbind(diag(l - 1), -1)
  }

  jacobian21 <- matrix(0, nrow = choose(num_items, 2),
                       ncol = sum(num_item_params))

  row_iterator <- 1

  for (jj in 1:num_items) {
    # the Jacobian matrix is calculated differently depending on the link
    # function; this describes the calculation for the logit link function
    # which is the most complicated
    #
    # multiply the prob of a correct response times prob of incorrect response
    # times design matrix times base rate times the prob correct for a second
    # item this estimates the covariance of the second-order marginal
    # probabilities by using the design matrix, this controls for parameters not
    # estimated for certain latent classes
    for (ii in 1:num_items) {
      if (jj < ii) {
        if (link == "logit") {
          jacobian21[row_iterator,
                     (cumulative_parameters[jj] -
                        num_item_params[jj] + 1):cumulative_parameters[jj]] <-
            colSums(pi_matrix[jj, ] * (1 - pi_matrix[jj, ]) *
                      design_matrix[[jj]][patt[jj, ], ] *
                      as.vector(base_rates) *
                      pi_matrix[ii, ])

          jacobian21[row_iterator,
                     (cumulative_parameters[ii] -
                        num_item_params[ii] + 1):cumulative_parameters[ii]] <-
            colSums(pi_matrix[ii, ] * (1 - pi_matrix[ii, ]) *
                      design_matrix[[ii]][patt[ii, ], ] *
                      as.vector(base_rates) *
                      pi_matrix[jj, ])
        } else if (link == "log") {
          jacobian21[row_iterator,
                     (cumulative_parameters[jj] -
                        num_item_params[jj] + 1):cumulative_parameters[jj]] <-
            colSums(pi_matrix[jj, ] *
                      design_matrix[[jj]][patt[jj, ], ] *
                      as.vector(base_rates) *
                      pi_matrix[ii, ])

          jacobian21[row_iterator,
                     (cumulative_parameters[ii] -
                        num_item_params[ii] + 1):cumulative_parameters[ii]] <-
            colSums(pi_matrix[ii, ] *
                      design_matrix[[ii]][patt[ii, ], ] *
                      as.vector(base_rates) *
                      pi_matrix[jj, ])
        } else if (link == "identity") {
          jacobian21[row_iterator,
                     (cumulative_parameters[jj] -
                        num_item_params[jj] + 1):cumulative_parameters[jj]] <-
            colSums(design_matrix[[jj]][patt[jj, ], ] * as.vector(base_rates) *
                      pi_matrix[ii, ])

          jacobian21[row_iterator,
                     (cumulative_parameters[ii] -
                        num_item_params[ii] + 1):cumulative_parameters[ii]] <-
            colSums(design_matrix[[ii]][patt[ii, ], ] * as.vector(base_rates) *
                      pi_matrix[jj, ])
        }


        row_iterator <- row_iterator + 1
      }
    }
  }

  # for jacobian12, we're calculating the difference in the prob of a correct
  # response between masters of all the attributes and the remaining latent
  # classes for each pair of items for a single attribute assessment, jacobian22
  # is just the prob of a correct response from nonmasters
  jacobian22_expected <- matrix(0, nrow = choose(num_items, 2), ncol = l)

  row_iterator <- 1

  for (jj in 1:num_items) {
    for (ii in 1:num_items) {
      if (jj < ii) {
        jacobian22_expected[row_iterator, ] <- pi_matrix[jj, ] *
          pi_matrix[ii, ]

        row_iterator <- row_iterator + 1
      }
    }
  }

  if (num_attr == 1) {
    jacobian22 <- matrix(utils::combn(jacobian12, 2, FUN = prod), ncol = 1)
  } else {
    jacobian22 <- jacobian22_expected %*% rbind(diag(l - 1), -1)
  }

  if (num_attr == 1) {
    jacobian <- cbind(rbind(jacobian11, jacobian21),
                      rbind(jacobian12, jacobian22) *
                        fisher_partial_p_alpha_l(base_rates))
  } else {
    jacobian <- cbind(rbind(jacobian11, jacobian21),
                      rbind(jacobian12, jacobian22))
  }

  return(jacobian)
}

#' Calculate the asymptotic covariance matrix
#'
#' Calculates the asymptotic covariance matrix
#'
#' @param num_items An integer of the number of items.
#' @param pi_matrix An item-by-class matrix containing the probability of a
#' correct response by members of each latent class
#' @param base_rates A single row matrix containing the structural parameters
#' indicating the model estimated base rates of mastery
#'
#' @noRd
calc_covariance_matrix <- function(num_items, pi_matrix, base_rates) {
  partitioned_cov_mat <- Mord(c(1:num_items), pi_matrix, base_rates)
  cov_mat <- cbind(rbind(partitioned_cov_mat$Xi11,
                         partitioned_cov_mat$Xi21),
                   rbind(t(partitioned_cov_mat$Xi21),
                         partitioned_cov_mat$Xi22))

  return(cov_mat)
}

#' Calculate the Design Matrix
#'
#' Calculates a design matrix for the attributes. Based off of de la Torre
#' (2011). The design matrix is a list with one matrix per item. For each item,
#' there is a binary class-by-parameter matrix that indicates which parameters
#' are estimated for each latent class.
#'
#' @param num_item_params A vector containing the number of estimated item
#' parameters for each of the items
#' @param qmatrix A data frame containing the Q-matrix
#' @param model_type A string containing the type of model (e.g., 'LCDM')
#'
#' @return `design_matrix` The design matrix.
#'
#' @references de la Torre, J. (2011). The generalized DINA model framework.
#'    *Psychometrika, 76*, 179-199. doi:10.1007/s11336-011-9207-7
#'
#' @noRd
calc_design_matrix <- function(num_item_params, qmatrix, model_type) {
  design_matrix <- list()

  for (ii in seq_len(nrow(qmatrix))) {
    if (sum(qmatrix[ii, ]) > 1) {
      design_matrix[[ii]] <- possible_parameters(sum(qmatrix[ii, ]),
                                                 model_type) %>%
        tibble::as_tibble(.name_repair = "unique_quiet") %>%
        only_if(model_type == "LCDM")(modelr::model_matrix)(
          ., stats::as.formula(paste0("~ .^", ncol(.)))
        ) %>%
        only_if(model_type %in% c("ACDM", "LLM", "RRUM"))(dplyr::mutate)(
          int = 1
        ) %>%
        only_if(model_type %in% c("ACDM", "LLM", "RRUM"))(dplyr::select)(
          "int", dplyr::everything()
        ) %>%
        as.matrix() %>%
        unname()
    } else {
      design_matrix[[ii]] <- matrix(nrow = 2^sum(qmatrix[ii, ]),
                                    ncol = num_item_params[ii])

      design_matrix[[ii]][, 1] <- 1
      design_matrix[[ii]][, 2] <- possible_parameters(sum(qmatrix[ii, ]),
                                                      model_type) %>%
        tibble::as_tibble(.name_repair = "unique_quiet") %>%
        only_if(model_type %in% c("DINO", "DINA", "BUGDINO"))(dplyr::select)(
          -"...1"
        ) %>%
        as.matrix() %>%
        unname()
    }
  }

  return(design_matrix)
}

#' Create a matrix containing all possible parameter combinations
#'
#' Calculates a binary class-by-parameter matrix that indicates which parameters
#' are estimated for each latent class.
#'
#' @param natt An integer containing the number of assessed attributes.
#' @param model_type A string containing the type of model (e.g., 'LCDM')
#'
#' @return `profiles` A class-by-parameter matrix.
#'
#' @noRd
possible_parameters <- function(natt, model_type) {
  attr_names <- as.vector(glue::glue("dplyr::desc(att_{1:natt})"))

  if (model_type %in% c("LCDM", "ACDM", "LLM", "RRUM")) {
    profiles <- rep(list(c(0L, 1L)), natt) %>%
      purrr::set_names(glue::glue("att_{seq_len(natt)}")) %>%
      expand.grid() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(total = rowSums(.)) %>%
      dplyr::select(dplyr::everything(), "total") %>%
      dplyr::arrange(.data$total, !!! rlang::parse_exprs(attr_names)) %>%
      dplyr::select(-"total") %>%
      as.matrix() %>%
      unname()
  } else if (model_type == "DINA") {
    profiles <- matrix(0, nrow = 2^natt, ncol = 2)

    profiles[, 1] <- 1
    profiles[2^natt, ] <- 1
  } else if (model_type == "DINO") {
    profiles <- matrix(1, nrow = 2^natt, ncol = 2)

    profiles[1, 2] <- 0
  } else if (model_type == "BUGDINO") {
    profiles <- matrix(0, nrow = 2^natt, ncol = 2)

    profiles[, 1] <- 1
    profiles[1, 2] <- 1
  }

  return(profiles)
}

#' Calculate the pattern matrix
#'
#' The pattern matrix is an item-by-class matrix. The entries of the pattern
#' matrix reflect congruence classes for the latent classes based on
#' which of the assessed attributes have been mastered by the examinees in each
#' of the latent classes.
#'
#' @param qmatrix A data frame containing the Q-matrix
#' @param l An integer of the number of possible attribute mastery patterns.
#' @param skills_missing A matrix of required skills that are missing by each
#' attribute class.
#'
#' @return `patt` The pattern matrix.
#'
#' @noRd
calc_patt <- function(qmatrix, l, skills_missing) {
  patt <- matrix(NA, nrow = nrow(qmatrix), ncol = l)

  for (mm in seq_len(nrow(qmatrix))) {
    for (nn in seq(2^rowSums(qmatrix)[mm])) {
      if (sum(qmatrix[mm, ]) == ncol(qmatrix)) {
        patt[mm, ] <- seq(2^rowSums(qmatrix)[mm])
      } else {
        val <- min(which(is.na(patt[mm, ])))
        patt[mm, ][skills_missing[mm, ] == skills_missing[mm, val]] <- nn
      }
    }
  }

  return(patt)
}

#' Calculate the skills missing for each item and attribute class
#'
#' @param base_rates A matrix of the base rates of attribute mastery.
#' @param l An integer of the number of possible attribute mastery patterns.
#' @param qmatrix A data frame containing the Q-matrix.
#'
#' @return `skills_missing` The item-by-class matrix indicating missing skills
#' for each latent class (e.g., '01' indicates the latent class is missing the
#' second skill).
#'
#' @noRd
skills <- function(base_rates, l, qmatrix) {
  skills_needed <- matrix(NA, nrow = nrow(qmatrix), ncol = l)
  skills_have <- matrix(NA, nrow = nrow(qmatrix), ncol = l)
  skills_missing <- matrix(NA, nrow = nrow(qmatrix), ncol = l)

  for (ii in seq_len(nrow(qmatrix))) {
    skills_have[ii, ] <- colnames(base_rates)

    skills_needed[ii, ] <- stringr::str_c(as.character(qmatrix[ii, ]),
                                          collapse = "")

    for (jj in 1:l) {
      temp <- ""
      for (kk in seq_len(ncol(qmatrix))) {
        if ((stringr::str_sub(skills_needed[ii, jj], kk, kk) == 1) &&
            (stringr::str_sub(skills_have[ii, jj], kk, kk) == 0)) {
          temp <- stringr::str_c(temp, "1")
        } else {
          temp <- stringr::str_c(temp, "0")
        }
      }
      skills_missing[ii, jj] <- temp
    }
  }

  return(skills_missing)
}

#' Calculate all possible item parameters
#'
#' @param natt The number of assessed attributes.
#'
#' @return `params` A vector containing all possible item parameters given
#' `natt` assessed attributes.
#'
#' @noRd
item_param_profiles <- function(natt) {

  mefs <- c(stringr::str_c("MEF", as.character(1:natt)))

  att_names <- glue::glue("att_{1:natt}") # nolint

  if (natt > 1) {
    ints <- as_binary(natt) %>%
      tibble::as_tibble(.name_repair = ~att_names) %>%
      dplyr::mutate(sum = rowSums(.)) %>%
      dplyr::filter(.data$sum > 1) %>%
      dplyr::select(-"sum") %>%
      tibble::rowid_to_column("item") %>%
      tidyr::pivot_longer(cols = -"item", names_to = "att",
                          values_to = "present") %>%
      dplyr::mutate(att = stringr::str_remove(.data$att, "att_")) %>%
      dplyr::filter(.data$present == 1) %>%
      dplyr::select(-"present") %>%
      dplyr::group_by(.data$item) %>%
      dplyr::mutate(param = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = "param", values_from = "att") %>%
      dplyr::select(-"item") %>%
      tidyr::unite(., col = "param", sep = "", remove = FALSE, na.rm = TRUE) %>%
      dplyr::select("param") %>%
      dplyr::mutate(param = stringr::str_c("Int", .data$param)) %>%
      dplyr::pull("param")
  } else {
    ints <- NULL
  }

  params <- c("Intercept", mefs, ints)

  return(params)
}

#' Fisher Partial Alpha
#'
#' Calculates the Fisher Partial Alpha statistic based on the base rates of
#' mastery.
#'
#' mastery and non-mastery.
#'
#' @noRd
fisher_partial_p_alpha_l <- function(base_rates) {
  l <- length(base_rates)
  d_value <- 1 / base_rates[l]
  alpha_l <- d_value * base_rates[-l]
  partial_alpha <- (d_value - alpha_l) / (d_value ^ 2)
  partial_alpha
}

#' Calculate Bivariate Probability Vector
#'
#' Calculates a vector of the bivariate probabilities of correct item responses.
#'
#' @param num_items An integer of the number of items.
#' @param bi A matrix of the probabilities bivariate correct responses.
#' @param pi_matrix An item-by-class matrix of the conditional response
#' probabilities.
#' @param base_rates A matrix of the base rates of attribute mastery.
#'
#' @noRd
calc_bivariate_prob <- function(num_items, bi, pi_matrix, base_rates) {
  for (ii in 1:num_items) {
    for (jj in 1:num_items) {
      if (ii >= jj) {
        bi[ii, jj] <- sum(t(pi_matrix)[, ii] * t(pi_matrix)[, jj] * base_rates)
      }
    }
  }

  return(bi)
}


#' Calculate Univariate Probability Vector
#'
#' Calculates a vector of the univariate probabilities of correct item
#' responses.
#'
#' @param num_items An integer for the number of items.
#' @param uni A vector of the univariate probabilities correct responses.
#' @param pi_matrix A matrix of the conditional response probabilities.
#' @param base_rates A matrix of the base rates of attribute mastery.
#'
#' @noRd
calc_univariate_prob <- function(num_items, uni, pi_matrix, base_rates) {
  for (ii in 1:num_items) {
    uni[ii] <- sum(t(pi_matrix)[, ii] * base_rates)
  }

  return(uni)
}

#' Calculate RMSEA from M2
#'
#' @param x2 The M2 statistic
#' @param df Degrees of freedom for the M2
#' @param n Sample size
#'
#' @noRd
rmsea_calc <- function(x2, df, n) {
  ret <- ifelse((x2 - df) > 0,
                sqrt(x2 - df) / sqrt(df * (n - 1)), 0)
  ret <- ifelse(is.na(ret), NaN, ret)
  ret
}


#' Calculate RMSEA confidence interval
#'
#' @param x2 The M2 statistic
#' @param df Degrees of freedom for the M2
#' @param n Sample size
#' @param ci_lower Lower end of confidence interval for RMSEA confidence
#'   interval
#' @param ci_upper Upper end of confidence interval for RMSEA confidence
#'   interval
#'
#' @noRd
rmsea_ci <- function(x2, df, n, ci_lower, ci_upper) {

  lower_lambda <- function(lambda) {
    stats::pchisq(x2, df = df, ncp = lambda) - ci_upper
  }
  upper_lambda <- function(lambda) {
    stats::pchisq(x2, df = df, ncp = lambda) - ci_lower
  }

  lambda_l <- try(stats::uniroot(f = lower_lambda, lower = 0, upper = x2)$root,
                  silent = TRUE)
  lambda_u <- try(stats::uniroot(f = upper_lambda, lower = 0,
                                 upper = max(n, x2 * 5))$root,
                  silent = TRUE)
  if (!methods::is(lambda_l, "try-error")) {
    rmsea_lower <- sqrt(lambda_l / (n * df))
  } else {
    rmsea_lower <- 0
  }
  if (!methods::is(lambda_u, "try-error")) {
    rmsea_upper <- sqrt(lambda_u / (n * df))
  } else {
    rmsea_upper <- 0
  }

  return(c(rmsea_lower, rmsea_upper))
}

#' Only If
#'
#' Adverb for conditionally skipping steps in a piped workflow.
#'
#' @param condition Logical condition to be evaluated
#' @examples
#' d <- tibble::as_tibble(mtcars)
#' d %>%
#'   only_if(TRUE)(dplyr::filter)(.data$mpg > 25)
#'
#' d %>%
#'   only_if(FALSE)(dplyr::filter)(.data$mpg > 25)
#' @author David Robinson, https://twitter.com/drob/status/785880369073500161
#' @noRd
only_if <- function(condition) {
  function(func) {
    if (condition) {
      func
    } else {
      function(., ...) .
    }
  }
}

#' Make Binary Profiles
#'
#' Given a number of attributes, `as_binary` will create all possible binary
#' mastery profiles.
#'
#' @param x The number of attributes
#'
#' @return A `2 ^ x` by `x` matrix
#' @export
#'
#' @examples
#' as_binary(3)
#' as_binary(4)
as_binary <- function(x) {
  attr_names <- as.vector(glue::glue("dplyr::desc(att_{1:x})"))

  profiles <- rep(list(c(0L, 1L)), x) %>%
    purrr::set_names(glue::glue("att_{seq_len(x)}")) %>%
    expand.grid() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(total = rowSums(.)) %>%
    dplyr::select(dplyr::everything(), "total") %>%
    dplyr::arrange(.data$total, !!! rlang::parse_exprs(attr_names)) %>%
    dplyr::select(-"total") %>%
    as.matrix() %>%
    unname()
  return(profiles)
}
