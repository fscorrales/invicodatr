test_that("reading rcocc31 returns a tibble", {
  df <- system.file("extdata", "rcocc31.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_mayor_contable_rcocc31()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 12)

})
