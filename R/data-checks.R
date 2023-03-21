check_data <- function(x) {
  if (!is.data.frame(x)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!tibble::is_tibble(x)) {
    tibble::as_tibble(x)
  } else {
    x
  }
}

check_ci <- function(x) {
  if (length(x) != 1 || !is.numeric(x)) {
    stop("`ci` must be a length one numeric vector.",
         call. = FALSE)
  }

  if (x <= 0 || x >= 1 || is.na(x)) {
    stop("`ci` must be between 0 and 1 and not missing.",
         call. = FALSE)
  } else {
    x
  }
}

check_data <- function(data, qmatrix) {
  if (!is.matrix(data)) {
    stop("`data` must be a matrix.",
         call. = FALSE)
  }

  if (nrow(data) < 1) {
    stop("`data` must include data for at least one student.",
         call. = FALSE)
  }

  if (ncol(data) != nrow(qmatrix)) {
    stop(paste("The number of items in `data` (i.e., the number of columns)",
               "must equal the number of items in the Q-matrix."),
         call. = FALSE)
  }

  if (!is.integer(data)) {
    stop("`data` must be of type integer.",
         call. = FALSE)
  }
}

check_struc_params <- function(struc_params, qmatrix) {
  if (!is.numeric(struc_params)) {
    stop("The class of `struc_params` must be numeric.",
         call. = FALSE)
  }

  if (length(struc_params) != 2^ncol(qmatrix)) {
    stop(paste("The length of `struc_params` does not match the number of",
               "latent classes indicated by `qmatrix`."),
         call. = FALSE)
  }

  if (!is.double(struc_params)) {
    stop("`struc_params` must be of type double",
         call. = FALSE)
  }
}

check_pi_matrix <- function(pi_matrix, qmatrix) {
  if (!is.matrix(pi_matrix)) {
    stop("`pi_matrix` must be a matrix.",
         call. = FALSE)
  }

  if (nrow(pi_matrix) != nrow(qmatrix)) {
    stop(paste("The number of items specific by `pi_matrix` and `qmatrix` do",
               "not match."),
         call. = FALSE)
  }

  if (ncol(pi_matrix) != 2^ncol(qmatrix)) {
    stop(paste("The number of latent classes specified in `pi_matrix` and",
               "`qmatrix` do not match."),
         call. = FALSE)
  }

  if (!is.double(pi_matrix)) {
    stop("`pi_matrix` must be of type double.",
         call. = FALSE)
  }
}

check_qmatrix <- function(qmatrix, pi_matrix) {
  if (!is.data.frame(qmatrix)) {
    stop("`qmatrix` must be a data frame.",
         call. = FALSE)
  }

  if (nrow(pi_matrix) != nrow(qmatrix)) {
    stop(paste("The number of items specific by `pi_matrix` and `qmatrix`",
               "do not match."),
         call. = FALSE)
  }

  if (ncol(pi_matrix) != 2^ncol(qmatrix)) {
    stop(paste("The number of latent classes specified in `pi_matrix` and",
               "`qmatrix` do not match."),
         call. = FALSE)
  }
}
