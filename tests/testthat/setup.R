possible_prof <- as_binary(ncol(sample_data$q_matrix))

fit_dat <- sample_data$data %>%
  tidyr::pivot_wider(names_from = "item_id",
                     values_from = "score") %>%
  dplyr::select(-"resp_id") %>%
  as.matrix() %>%
  unname()
