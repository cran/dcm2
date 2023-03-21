test_that("methods work", {
  out <- utils::capture.output(
    gdina_mod <- GDINA::GDINA(dat = fit_dat,
                              Q = data.frame(sample_data$q_matrix),
                              model = "logitGDINA",
                              control = list(conv.type = "neg2LL")))

  gdina_m2 <- GDINA::modelfit(gdina_mod)
  dcm2_m2 <- fit_m2(gdina_mod, ci = 0.9)

  expect_equal(gdina_m2$M2, dcm2_m2$m2, tolerance = 0.01)
  expect_equal(gdina_m2$M2.df, dcm2_m2$df)
  expect_equal(gdina_m2$M2.pvalue, dcm2_m2$pval, tolerance = 0.01)
  expect_equal(gdina_m2$RMSEA2, dcm2_m2$rmsea, tolerance = 0.01)
  expect_equal(gdina_m2$RMSEA2.CI[1], dcm2_m2$ci_lower, tolerance = 0.01)
  expect_equal(gdina_m2$RMSEA2.CI[2], dcm2_m2$ci_upper, tolerance = 0.01)
  expect_equal(gdina_m2$SRMSR, dcm2_m2$srmsr, tolerance = 0.01)
})
