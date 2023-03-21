#' dcm2: A package for the estimating the M2 statistic for DCMs
#'
#' @useDynLib dcm2, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @keywords internal
"_PACKAGE"

## Make R CMD Check go away
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
