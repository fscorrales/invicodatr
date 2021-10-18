test_that("reading rf602 returns a tibble", {
  df <- system.file("extdata", "rf602.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_ppto_gtos_fte_rf602()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 15)

})

test_that("reading rf610 returns a tibble", {
  df <- system.file("extdata", "rf610.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_ppto_gtos_desc_rf610()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 18)

})
