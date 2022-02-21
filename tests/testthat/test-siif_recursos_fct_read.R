test_that("reading rci02 returns a tibble", {
  df <- system.file("extdata", "rci02.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_comprobantes_rec_rci02()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 13)

})

test_that("reading ri102 returns a tibble", {
  df <- system.file("extdata", "ri102.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_ppto_rec_ri102()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 10)

})
