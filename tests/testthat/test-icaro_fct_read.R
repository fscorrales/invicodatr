test_that("converting ICARO's OBRAS table returns a tibble", {
  df <- system.file("extdata", "ICARO.sqlite",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_icaro_old_obras()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 11)

})

test_that("converting ICARO's CARGA table returns a tibble", {
  df <- system.file("extdata", "ICARO.sqlite",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_icaro_old_carga()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 14)

})

test_that("converting ICARO's PROGRAMAS table returns a tibble", {
  df <- system.file("extdata", "ICARO.sqlite",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_icaro_old_programas()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 2)

})

test_that("converting ICARO's SUBPROGRAMAS table returns a tibble", {
  df <- system.file("extdata", "ICARO.sqlite",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_icaro_old_subprogramas()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 3)

})

test_that("converting ICARO's PROYECTOS table returns a tibble", {
  df <- system.file("extdata", "ICARO.sqlite",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_icaro_old_proyectos()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 3)

})

test_that("converting ICARO's ACTIVIDADES table returns a tibble", {
  df <- system.file("extdata", "ICARO.sqlite",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_icaro_old_actividades()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 3)

})

test_that("converting ICARO's CUENTASBANCARIAS table returns a tibble", {
  df <- system.file("extdata", "ICARO.sqlite",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_icaro_old_cta_cte()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 4)

})

test_that("converting ICARO's PROVEEDORES table returns a tibble", {
  df <- system.file("extdata", "ICARO.sqlite",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_icaro_old_proveedores()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 7)

})

test_that("converting ICARO's RETENCIONES table returns a tibble", {
  df <- system.file("extdata", "ICARO.sqlite",
                    package = "invicodatr", mustWork = TRUE) %>%
    read_icaro_old_retenciones()
  expect_s3_class(df, c("tbl_df", "tbl", "data.frame"))
  expect_equal(ncol(df), 4)

})
