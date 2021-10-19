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

test_that("reading rcg01_uejp returns a tibble", {
  df <- system.file("extdata", "rcg01_uejp.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_comprobantes_gtos_rcg01_uejp()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 18)

})

test_that("reading rcg01_par returns a tibble", {
  df <- system.file("extdata", "rcg01_par.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_comprobantes_gtos_partida_rcg01_par()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 18)

})

test_that("reading gto_rpa03g returns a tibble", {
  df <- system.file("extdata", "gto_rpa03g.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_comprobantes_gtos_gpo_partida_gto_rpa03g()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 11)

})
