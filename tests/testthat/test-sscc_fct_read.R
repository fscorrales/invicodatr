test_that("reading sscc_banco returns a tibble", {
  df <- system.file("extdata", "sscc_banco.csv",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_sscc_banco_invico()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 13)

})
