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
  expect_equal(ncol(df), 19)

})

test_that("reading rcg01_par returns a tibble", {
  df <- system.file("extdata", "rcg01_par.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_comprobantes_gtos_partida_rcg01_par()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 19)

})

test_that("reading gto_rpa03g returns a tibble", {
  df <- system.file("extdata", "gto_rpa03g.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_comprobantes_gtos_gpo_partida_gto_rpa03g()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 11)

})

test_that("reading rao01 returns a tibble", {
  df <- system.file("extdata", "rao01.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_retenciones_por_codigo_rao01()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 7)

})

test_that("reading rfondo07tp returns a tibble", {
  df <- system.file("extdata", "rfondo07tp.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_resumen_fdos_rfondo07tp()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 8)

})

test_that("reading rdeu012 returns a tibble", {
  df <- system.file("extdata", "rdeu012.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_deuda_flotante_rdeu012()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 15)

})

test_that("reading rdeu012b2_c returns a tibble", {
  df <- system.file("extdata", "rdeu012b2_c.csv",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_deuda_flotante_tg_rdeu012b2_c()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 14)

})

test_that("reading rtr03 returns a tibble", {
  df <- system.file("extdata", "rtr03.xls",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_siif_pagos_rtr03()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 13)

})
