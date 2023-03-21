#' @title Simulated Data for Testing Functions
#'
#' @description A matrix with randomly simulated data to test the package
#' functions.
#'
#' @format A list frame containing 4 \code{tibble} objects:
#' *  `resp_profiles`: A \code{tibble} with 1000 rows and 3 columns. The first
#' column indicates `resp_id` (i.e., the respondent identification number).
#' The second column indicates `att_1` (i.e., a binary indicator for whether the
#' respondent mastered the first attribute). The third column indicates `att_2`
#' (i.e., a binary indicator for whether the respondent mastered the second
#' attribute).
#' * `q_matrix`: A \code{tibble} with 8 rows and 2 columns. Each row corresponds
#' to an assessment item, and the column entries provide a binary indicator for
#' whether the item assessed each of the attribute.
#' * `item_params`: A \code{tibble} with 8 rows and 5 columns. Each row
#' corresponds to an item. The first column indicates `item_id` (i.e., the item
#' identification number). The second column indicates `intercept` (i.e., the
#' true item intercept parameter for the item). The third column indicates
#' `att_1` (i.e., the true item main effect parameter for the first attribute
#' for the item). The fourth column indicates `att_2` (i.e., the true item main
#' effect parameter for the second attribute for the item). The fifth column
#' indicates `att_1__att_2` (i.e., the true item interaction effect parameter
#' for the first and second attributes).
#' * `data`: A \code{tibble} with 8000 rows and 3 columns. The first column
#' indicates `resp_id` (i.e., the respondent identification number). The second
#' column indicates `item_id` (i.e., the item identification number). The third
#' column indicates `score` (i.e., the dichotomously scored item response).
"sample_data"
