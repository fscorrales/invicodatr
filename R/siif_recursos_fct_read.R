#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

read_siif_comprobantes_rec_rci02 <- function(path){

  required_ext <- "xls"
  required_ncol <- 43
  required_title <- "RESUMEN DIARIO DE COMPROBANTES DE RECURSOS"
  required_nvar <- 13

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  suppressMessages(
    db <- readxl::read_excel(path,
                             col_types = "text",
                             col_names = FALSE)
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  read_title <- (db$...24[5])

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  db <- db %>%
    dplyr::mutate(ejercicio = ...33[2])

  db <- db %>%
    dplyr::select(.data$ejercicio, ...1, ...5, ...9, ...12,
                  ...16, ...22, ...27, ...31, ...41)

  names(db) <- c("ejercicio", "nro_entrada", "fuente",
                 "clase_reg", "clase_mod", "fecha",
                 "monto", "cta_cte", "glosa", "verificado")

  db <- utils::tail(db, -20) %>%
    dplyr::filter(.data$nro_entrada != is.na(.data$nro_entrada))

  db <- db %>%
    dplyr::mutate(fecha = zoo::as.Date(readr::parse_integer(.data$fecha),
                                       origin = "1899-12-30"),
                  mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha),
                                                        2, pad = "0"),
                                       lubridate::year(.data$fecha), sep = "/"),
                  nro_entrada = readr::parse_integer(.data$nro_entrada),
                  monto = readr::parse_number(.data$monto,
                                              locale = readr::locale(decimal_mark = ".")),
                  remanente = ifelse(stringr::str_detect(.data$glosa,
                                                         "REMANENTE"), T, F),
                  invico = ifelse(stringr::str_detect(.data$glosa, "3*%"),
                                  T, F))

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_siif_ppto_rec_ri102 <- function(path){

  required_ext <- "xls"
  required_ncol <- 28
  required_title <- "ri102"
  required_nvar <- 10

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  suppressMessages(
    db <- readxl::read_excel(path,
                             col_types = "text",
                             col_names = FALSE)
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  read_title <- (db$...26[8])

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  db <- db %>%
    dplyr::mutate(ejercicio = ...16[4])

  db <- db %>%
    dplyr::select(.data$ejercicio, ...1, ...3, ...10, ...11,
                  ...13, ...14, ...18, ...21, ...24)

  names(db) <- c("ejercicio", "cod_rec", "descripcion",
                 "fuente", "org_fin", "ppto_inicial", "ppto_modif",
                 "ppto_vigente", "ingreso", "saldo")

  db <- utils::tail(db, -18) %>%
    dplyr::filter(.data$cod_rec != is.na(.data$cod_rec))

  db <- db %>%
    dplyr::mutate_at(c("ppto_inicial", "ppto_modif", "ppto_vigente",
                       "ingreso", "saldo"),
                     readr::parse_number,
                     locale = readr::locale(decimal_mark = "."))

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}
