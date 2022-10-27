read_sgf_resumen_rend_prov <- function(path){

  required_ext <- "csv"
  # required_ncol <- 33
  required_title <- "Resumen de Rendiciones (Detalle)"
  required_nvar <- 20

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
                  ejercicio = as.character(lubridate::year(.data$fecha)),
                  mes =  stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha), 2, pad = "0"),
                                        lubridate::year(.data$fecha), sep = "/"),
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

read_sgf_resumen_rend_obra <- function(path){

  required_ext <- "csv"
  # required_ncol <- 33
  required_title <- "Resumen de Rendiciones (por Obras)"
  required_nvar <- 19

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

  read_title <- stringr::str_sub(db$X2[1], 1, 34)

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  origen_vec <- stringr::str_split(db$X7[1]," - ", simplify = T)[1]
  origen_vec <- stringr::str_split(origen_vec, " = ", simplify = T)[2]
  origen_vec <- stringr::str_remove_all(origen_vec, '\\"')

  names_vec <- c("obra" ,"origen", "beneficiario", "libramiento_sgf", "destino", "fecha",
                 "movimiento", "importe_neto", "gcias", "sellos", "tl","iibb",
                 "suss", "seguro", "salud", "mutual", "importe_bruto")

  db_mod <- purrr::map_dfc(names_vec, stats::setNames,
                           object = list(character()))

  # db <- db %>%
  #   dplyr::mutate(origen = origen_vec)

  if (origen_vec == "EPAM") {
    db <- db %>%
      dplyr::select(-X1:-X25) %>%
      dplyr::mutate(obra = ifelse(is.na(X54), NA, X26)) %>%
      dplyr::transmute(origen = origen_vec,
                      obra = zoo::na.locf(.data$obra, na.rm = F),
                      obra = ifelse(!is.na(.data$obra), .data$obra, X39),
                      beneficiario = ifelse(is.na(X54), X26, X37),
                      libramiento_sgf = ifelse(is.na(X54), X27, X38),
                      destino = ifelse(is.na(X54), X28, X39),
                      fecha = ifelse(is.na(X54), X29, X40),
                      movimiento = ifelse(is.na(X54), X30, X41),
                      importe_neto = ifelse(is.na(X54), X31, X42),
                      gcias = ifelse(is.na(X54), X32, X43),
                      sellos = ifelse(is.na(X54), X33, X44),
                      tl = ifelse(is.na(X54), X34, X45),
                      iibb = ifelse(is.na(X54), X35, X46),
                      suss = ifelse(is.na(X54), X36, X47),
                      seguro = ifelse(is.na(X54), X37, X48),
                      salud = ifelse(is.na(X54), X38, X49),
                      mutual = ifelse(is.na(X54), X39, X50),
                      importe_bruto = ifelse(is.na(X54), X40, X51))
      # dplyr::select(-X41:-X64) %>%
      # dplyr::rename(beneficiario = X26, libramiento_sgf = X27, destino = X28,
      #               fecha = X29, movimiento = X30,
      #               importe_neto = X31, gcias = X32, sellos = X33,
      #               tl = X34, iibb = X35, suss = X36, seguro = X37,
      #               salud = X38, mutual = X39, importe_bruto = X40)

    db <- db_mod %>%
      dplyr::full_join(db, by = colnames(db))

    db <- db %>%
      dplyr::mutate(fecha = lubridate::dmy(.data$fecha),
                    ejercicio = as.character(lubridate::year(.data$fecha)),
                    mes =  stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha), 2, pad = "0"),
                                          lubridate::year(.data$fecha), sep = "/"),
                    obra = ifelse(.data$obra == "0.00",
                                  .data$destino, .data$obra),
                    movimiento = ifelse(.data$movimiento == "2",
                                        "DEBITO", .data$movimiento)) %>%
      dplyr::mutate_at(c("importe_neto", "gcias", "sellos", "tl", "iibb",
                         "suss", "seguro", "salud", "mutual", "importe_bruto"),
                       ~round(readr::parse_number(.), 2)) %>%
      dplyr::select(.data$ejercicio, .data$mes, .data$origen,
                    .data$obra, dplyr::everything())

  } else {
    # db <- db %>%
    #   dplyr::select(-X1:-X23) %>%
    #   dplyr::select(-X37:-X47) %>%
    #   dplyr::rename(beneficiario = X24, cta_cte = X25,
    #                 libramiento_sgf = X26, fecha = X27, movimiento = X28,
    #                 importe_neto = X36, gcias = X30, sellos = X31,
    #                 iibb = X32, suss = X33,
    #                 invico = X34, otras = X35, importe_bruto = X29)

  }

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
    dplyr:: mutate(codigo = readr::parse_integer(.data$codigo)) %>%
    dplyr::filter(!is.na(.data$cuit))

  db$cuit <- gsub('-', '', db$cuit)

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}
