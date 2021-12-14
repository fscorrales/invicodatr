#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

read_slave_honorarios <- function(path){

  required_ext <- "xlsx"
  required_ncol <- 14
  # required_title <- "DETALLES DE MOVIMIENTOS CONTABLES"
  required_nvar <- 17

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  suppressMessages(
    db <- readxl::read_excel(path,
                             col_types = "text",
                             col_names = TRUE)
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  # read_title <- stringr::str_sub(db$...2[8], 1, 33)
  #
  # if (is.na(read_title) | (read_title != required_title)) {
  #   abort_bad_title(path, read_title, required_title)
  # }

  names(db) <- c("fecha", "beneficiario", "sellos", "seguro",
                 "nro_entrada", "tipo", "importe_bruto",
                 "iibb", "lp", "otras_ret", "anticipo",
                 "descuento", "actividad", "partida")

  db <- db %>%
    dplyr::mutate(fecha = zoo::as.Date(readr::parse_integer(.data$fecha),
                                       origin = "1899-12-30"),
                  ejercicio = as.character(lubridate::year(.data$fecha)),
                  mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha), 2, pad = "0"),
                                       lubridate::year(.data$fecha), sep = "/"),
                  estructura = paste0(stringr::str_sub(.data$actividad, 0, 2),
                                     "-00", stringr::str_sub(.data$actividad, -6))) %>%
    dplyr::mutate_at(c("sellos", "seguro", "importe_bruto", "iibb", "lp",
                       "otras_ret", "anticipo", "descuento"),
                     ~round(readr::parse_number(.), 2)) %>%
    dplyr::mutate(importe_neto = .data$importe_bruto - .data$sellos - .data$seguro -
                    .data$iibb, .data$lp, .data$otras_ret, .data$anticipo, .data$descuento,
                  importe_neto = round(.data$importe_neto, 2)) %>%
    dplyr::filter(!stringr::str_detect(.data$nro_entrada, "NoSIIF")) %>%
    dplyr::select(-.data$actividad) %>%
    dplyr::select(.data$ejercicio, .data$mes, .data$fecha, .data$nro_entrada,
                  .data$tipo, .data$beneficiario, .data$estructura, .data$partida,
                  .data$importe_bruto, .data$importe_neto, dplyr::everything())

  #Arreglo temporal hasta crear Slave.R
  db <- db %>%
    dplyr::mutate(
      nro_entrada = dplyr::case_when(
        nro_entrada == "00917/21" ~ "00918/21",
        nro_entrada == "00000/21" ~ "02040/21",
        TRUE ~ nro_entrada
      )
    )

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}
