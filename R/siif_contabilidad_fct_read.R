#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

read_siif_mayor_contable_rcocc31 <- function(path){

  required_ext <- "xls"
  required_ncol <- 33
  required_title <- "DETALLES DE MOVIMIENTOS CONTABLES"
  required_nvar <- 11

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

  read_title <- stringr::str_sub(db$...2[8], 1, 33)

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  db <- db %>%
    dplyr::mutate(ejercicio = stringr::str_sub(...1[2], -4),
                  cta_contable = paste(...6[9], ...11[9], ...12[9], sep = "-"))

  db <- db %>%
    dplyr::select(.data$cta_contable, .data$ejercicio, ...3, ...9, ...14,
                  ...19, ...21, ...24, ...25, ...27)
  names(db) <- c("cta_contable", "ejercicio", "nro_entrada", "fecha_aprobado",
                 "auxiliar_1", "auxiliar_2", "tipo_comprobante",
                 "debitos", "creditos", "saldo")
  db <- utils::tail(db, -18) %>%
    dplyr::filter(.data$nro_entrada != is.na(.data$nro_entrada))
  #Null en Auxiliar 2?

  db <- db %>%
    dplyr::mutate(fecha_aprobado = zoo::as.Date(readr::parse_integer(.data$fecha_aprobado),
                                                origin = "1899-12-30"),
                  fecha = dplyr::if_else(lubridate::year(.data$fecha_aprobado) == .data$ejercicio,
                                         .data$fecha_aprobado,
                                         lubridate::dmy(stringr::str_c("31/12/", .data$ejercicio))),
                  nro_entrada = readr::parse_integer(.data$nro_entrada),
                  debitos = readr::parse_double(.data$debitos),
                  creditos = readr::parse_double(.data$creditos),
                  saldo = readr::parse_double(.data$saldo))

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}
