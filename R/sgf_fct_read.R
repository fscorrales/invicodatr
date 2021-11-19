read_sgf_resumen_rend_prov <- function(path){

  required_ext <- "csv"
  # required_ncol <- 33
  required_title <- "Resumen de Rendiciones (Detalle)"
  required_nvar <- 18

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

  # read_ncol <- ncol(db)
  #
  # if (read_ncol != required_ncol) {
  #   abort_bad_ncol(path, read_ncol, required_ncol)
  # }

  read_title <- stringr::str_sub(db$X2[1], 1, 32)

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  origen_vec <- stringr::str_split(db$X7[1]," - ", simplify = T)[1]
  origen_vec <- stringr::str_split(origen_vec, " = ", simplify = T)[2]
  origen_vec <- stringr::str_remove_all(origen_vec, '\\"')

  names_vec <- c("origen", "beneficiario", "cta_cte", "libramiento_sgf", "fecha",
                 "movimiento", "importe_neto", "gcias", "sellos", "iibb",
                 "suss", "invico", "otras", "importe_bruto", "destino",
                 "seguro", "salud", "mutual")

  db_mod <- purrr::map_dfc(names_vec, stats::setNames,
                           object = list(character()))

  db <- db %>%
    dplyr::mutate(origen = origen_vec)

  if (origen_vec == "OBRAS") {
    db <- db %>%
      dplyr::select(-X1:-X23) %>%
      dplyr::select(-X37:-X47) %>%
      dplyr::rename(beneficiario = X24, cta_cte = X25,
                    libramiento_sgf = X26, fecha = X27, movimiento = X28,
                    importe_neto = X36, gcias = X30, sellos = X31,
                    iibb = X32, suss = X33,
                    invico = X34, otras = X35, importe_bruto = X29)

  } else {
    db <- db %>%
      dplyr::select(-X1:-X26) %>%
      dplyr::select(-X42:-X53) %>%
      dplyr::rename(beneficiario = X27, destino = X28, cta_cte = X29,
                    libramiento_sgf = X30, fecha = X31, movimiento = X32,
                    importe_neto = X33, gcias = X34, sellos = X35,
                    iibb = X36, suss = X37, seguro = X38,
                    salud = X39, mutual = X40, importe_bruto = X41)
  }

  db <- db_mod %>%
    dplyr::full_join(db, by = colnames(db))

  db <- db %>%
    dplyr::select(.data$origen, dplyr::everything()) %>%
    dplyr::mutate(fecha = lubridate::dmy(.data$fecha),
                  movimiento = ifelse(.data$movimiento == "TRANSF.",
                                      "DEBITO", .data$movimiento),
                  cta_cte = ifelse(is.na(.data$cta_cte) & .data$beneficiario == "CREDITO ESPECIAL",
                                   "130832-07", .data$cta_cte),
                  cta_cte = ifelse(.data$cta_cte == "71-1-10270-5", "22110270-05",
                                   .data$cta_cte)) %>%
    dplyr::mutate_at(c("importe_neto", "gcias", "sellos", "iibb",
                       "suss", "salud", "mutual", "importe_bruto",
                       "invico", "otras", "seguro"),
                     ~round(readr::parse_number(.), 2))

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_sgf_listado_prov <- function(path){

  required_ext <- "csv"
  required_ncol <- 18
  required_title <- "Listado de Proveedores"
  required_nvar <- 7

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  suppressMessages(
    db <- vroom::vroom(path, col_names = FALSE, delim = ",",
                       col_types = vroom::cols(.default = "c"),
                       locale = vroom::locale(encoding = 'ISO-8859-1'))
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  read_title <- db$X2[1]

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  db <- db[,10:16]
  names(db) <- c("codigo","descripcion","domicilio", "localidad",
                 "telefono","cuit","condicion_iva")

  db <- db %>%
    dplyr:: mutate(codigo = readr::parse_integer(.data$codigo))

  db$cuit <- gsub('-', '', db$cuit)

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}
