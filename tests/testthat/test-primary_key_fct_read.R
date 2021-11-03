test_that("reading xlsx cta cte returns a tibble", {
  df <- system.file("extdata", "primary_key_cta_cte.xlsx",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_cta_cte()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 9)

})
