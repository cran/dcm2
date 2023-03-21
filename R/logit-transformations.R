#' Log-odds Transformation
#'
#' These functions implement the log-odds (or logit) transformation. This is a
#' common transformation for psychometric models that is used to put
#' probabilities on a continuous scale.
#'
#' @param x A number to be transformed
#'
#' @return A transformed double
#'
#' @examples
#' logit(0.6)
#' logit(0.5)
#'
#' inv_logit(3.5)
#' inv_logit(0)
#'
#' @name log_odds
NULL

#' @rdname log_odds
#' @export
logit <- function(x) {
  x <- check_logit_x(x)
  log(x / (1 - x))
}

#' @rdname log_odds
#' @export
inv_logit <- function(x) {
  x <- check_invlogit_x(x)
  exp(x) / (1 + exp(x))
}

check_logit_x <- function(x) {
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector", call. = FALSE)
  }

  if (is.na(x) || x >= 1 || x <= 0) {
    stop("`x` must be between 0 and 1 and non-missing", call. = FALSE)
  } else {
    x
  }
}

check_invlogit_x <- function(x) {
  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector", call. = FALSE)
  }

  if (is.na(x)) {
    stop("`x` must not be missing", call. = FALSE)
  } else {
    x
  }
}
