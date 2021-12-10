test_that("reading honorarios slave xls returns a tibble", {
  df <- system.file("extdata", "honorarios_slave.xlsx",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_slave_honorarios()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 16)

})
