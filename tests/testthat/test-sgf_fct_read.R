test_that("reading resumen_rend_prov returns a tibble", {
  df <- system.file("extdata", "resumen_rend_prov.csv",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_sgf_resumen_rend_prov()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 18)

})
