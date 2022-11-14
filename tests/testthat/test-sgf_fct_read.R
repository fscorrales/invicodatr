test_that("reading resumen_rend_prov_epam returns a tibble", {
  df <- system.file("extdata", "resumen_rend_prov_epam.csv",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_sgf_resumen_rend_prov()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 20)

})

test_that("reading resumen_rend_prov_obras returns a tibble", {
  df <- system.file("extdata", "resumen_rend_prov_obras.csv",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_sgf_resumen_rend_prov()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 20)

})

test_that("reading resumen_rend_obra returns a tibble", {
  df <- system.file("extdata", "resumen_rend_obra.csv",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_sgf_resumen_rend_obra()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 19)

})

test_that("reading listado_prov returns a tibble", {
  df <- system.file("extdata", "listado_prov.csv",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_sgf_listado_prov()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 7)

})
