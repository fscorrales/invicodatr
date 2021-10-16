test_that("reader returns a tibble", {
  df <- system.file("extdata", "2010-rf602.xls", package = "invicodatr") %>%
    read_siif_ppto_gtos_fte_rf602()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))

})
