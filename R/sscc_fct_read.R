read_sscc_banco_invico <- function(path){

  required_ext <- "csv"
  required_ncol <- 33
  required_title <- "Consulta General de Movimientos"
  required_nvar <- 13

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  # db <- readr::read_csv(x, col_names = FALSE,
  #                       col_types = readr::cols(.default = "c"),
  #                       locale = readr::locale(encoding = stringi::stri_enc_get()))

  suppressMessages(
    db <- vroom::vroom(path, col_names = FALSE, delim = ",",
                       col_types = vroom::cols(.default = "c"),
                       locale = vroom::locale(encoding = 'ISO-8859-1'))
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  read_title <- (db$X2[1])

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  db <- db %>%
    dplyr::select(X21:X29)

  names(db) <- c("fecha","movimiento","cta_cte", "concepto",
                 "beneficiario","moneda","libramiento",
                 "imputacion", "monto")
  db <- db %>%
    dplyr::mutate_all(stringr::str_replace_all,
                      pattern = "[\r\n]", replacement = "") %>%
    dplyr::mutate(fecha = lubridate::dmy(.data$fecha),
                  ejercicio = as.character(lubridate::year(.data$fecha)),
                  monto = round(readr::parse_number(.data$monto), 2),
                  libramiento = ifelse(is.na(.data$libramiento),
                                       "", .data$libramiento), #Decidí reemplazar NAs con ""
                  codigo_imputacion = readr::parse_integer(stringr::str_sub(.data$imputacion,
                                                                            1, 3)),
                  imputacion = stringr::str_sub(.data$imputacion, 5),
                  es_cheque =  !is.na(suppressWarnings(as.numeric(.data$movimiento))),
                  mes = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha),
                                                        2, pad = "0"),
                                       lubridate::year(.data$fecha), sep = "/"))

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}
