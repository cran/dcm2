#' @title Simulated Data for a Single Attribute Assessment
#'
#' @description A list containing data from a randomly simulated
#' single-attribute assessment.
#'
#' @format A list frame containing 4 \code{tibble} objects:
#' *  `resp_profiles`: A \code{tibble} with 1000 rows and 2 columns. The first
#' column indicates `resp_id` (i.e., the respondent identification number) and
#' the second column indicates `att_1` (i.e., a binary indicator for whether the
#' respondent mastered the first attribute).
#' * `q_matrix`: A \code{tibble} with 2 rows and 1 column. Each row corresponds
#' to an assessment item, and the column entries provide a binary indicator for
#' whether the item assessed the attribute.
#' * `item_params`: A \code{tibble} with 2 rows and 3 columns. Each row
#' corresponds to an item. The first column indicates `item_id` (i.e., the item
#' identification number). The second column indicates `intercept` (i.e., the
#' true item intercept parameter for the item). The third column indicates
#' `att_1` (i.e., the true item main effect parameter for the item).
#' * `data`: A \code{tibble} with 2000 rows and 3 columns. The first column
#' indicates `resp_id` (i.e., the respondent identification number). The second
#' column indicates `item_id` (i.e., the item identification number). The third
#' column indicates `score` (i.e., the dichotomously scored item response).
"data_att1"
