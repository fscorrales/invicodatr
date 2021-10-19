#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

read_siif_ppto_gtos_fte_rf602 <- function(path){

  required_ext <- "xls"
  required_ncol <- 23
  required_title <- "DETALLE DE LA EJECUCION PRESUESTARIA"
  required_nvar <- 15

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

  read_title <- (db$...1[4])
  read_title <- stringr::str_sub(read_title, 1,
                                  (stringr::str_length(read_title) - 5))

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  db <- db %>%
    dplyr::transmute(ejercicio = stringr::str_sub(...1[4], -4),
                     programa = stringr::str_pad(...1, 2, pad = "0"),
                     subprograma = stringr::str_pad(...2, 2, pad = "0"),
                     proyecto = stringr::str_pad(...5, 2, pad = "0"),
                     actividad = stringr::str_pad(...6, 2, pad = "0"),
                     partida = ...7,
                     grupo = stringr::str_c(stringr::str_sub(.data$partida, 1,1), "00", ""),
                     fuente = ...8,
                     org = ...9,
                     credito_original = ...12,
                     credito_vigente = ...13,
                     comprometido = ...14,
                     ordenado = ...15,
                     saldo = ...17,
                     pendiente = ...19) %>%
    utils::tail(-13) %>%
    dplyr::filter(.data$programa != is.na(.data$programa)) %>%
    dplyr::mutate_at(c("credito_original", "credito_vigente", "comprometido",
                       "ordenado", "saldo", "pendiente"),
                     readr::parse_number,
                     locale = readr::locale(decimal_mark = "."))

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_siif_ppto_gtos_desc_rf610 <- function(path){

  required_ext <- "xls"
  required_ncol <- 65
  required_title <- "LISTADO DE EJECUCION DE GASTOS POR PARTIDA"
  required_nvar <- 18

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

  read_title <- stringr::str_c(db$...32[1], db$...32[3], sep = " ")

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  db <- db %>%
    dplyr::transmute(ejercicio = stringr::str_sub(...33[8], -4),
                     programa = ...5,
                     subprograma = ...9,
                     proyecto = ...14,
                     actividad =  ...17,
                     grupo = ...20,
                     partida = ...21,
                     desc_part = ...24,
                     credito_original = ...38,
                     credito_vigente = ...44,
                     comprometido = ...49,
                     ordenado = ...55,
                     saldo = ...60) %>%
    utils::tail(-28) %>%
    dplyr::mutate(programa = zoo::na.locf(.data$programa),
                  subprograma = zoo::na.locf(.data$subprograma, F),
                  proyecto = zoo::na.locf(.data$proyecto, F),
                  actividad = zoo::na.locf(.data$actividad, F),
                  grupo = zoo::na.locf(.data$grupo, F),
                  partida = zoo::na.locf(.data$partida, F),
                  desc_part = zoo::na.locf(.data$desc_part, F)) %>%
    dplyr::filter(.data$credito_original != is.na(.data$credito_original)) %>%
    dplyr::mutate_at(c("credito_original", "credito_vigente",
                       "comprometido", "ordenado", "saldo"),
                     readr::parse_number,
                     locale = readr::locale(decimal_mark = ".")) %>%
    tidyr::separate(.data$programa, c("programa", "desc_prog"),
                    remove = T, extra = "merge") %>%
    tidyr::separate(.data$subprograma, c("subprograma", "desc_subprog"),
                    remove = T, extra = "merge") %>%
    tidyr::separate(.data$proyecto, c("proyecto", "desc_proy"),
                    remove = T, extra = "merge") %>%
    tidyr::separate(.data$actividad, c("actividad", "desc_act"),
                    remove = T, extra = "merge") %>%
    tidyr::separate(.data$grupo, c("grupo", "desc_gpo"),
                    remove = T, extra = "merge") %>%
    dplyr::mutate_at(c("programa", "subprograma",
                       "proyecto", "actividad"),
                     stringr::str_pad, width = 2, pad = "0")

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_siif_comprobantes_gtos_rcg01_uejp <- function(path){

  required_ext <- "xls"
  required_ncol <- 19
  required_title <- "Resumen Diario de Comprobantes de Gastos Ingresados"
  required_nvar <- 18

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

  read_title <- (db$...1[4])

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  db <- db %>%
    dplyr::mutate(ejercicio = stringr::str_sub(...1[2], -4)) %>%
    dplyr::select(-...18, -...19)

  names(db) <- c("nro_entrada", "nro_origen", "fuente", "clase_reg",
                 "clase_mod", "clase_gto", "fecha", "monto",
                 "cuit", "beneficiario", "nro_expte","cta_cte",
                 "comprometido", "verificado", "aprobado", "pagado",
                 "nro_fondo", "ejercicio")

  db <- db %>%
    utils::tail(-15) %>%
    dplyr::filter(.data$cuit != is.na(.data$cuit)) %>%
    dplyr::filter(.data$nro_entrada != is.na(.data$nro_entrada)) %>%
    dplyr::mutate(fecha = as.Date(readr::parse_integer(.data$fecha),
                                  origin = "1899-12-30"),
                  nro_entrada = readr::parse_integer(.data$nro_entrada),
                  nro_origen = readr::parse_integer(.data$nro_origen),
                  monto = readr::parse_number(.data$monto,
                                              locale = readr::locale(decimal_mark = ".")),
                  comprometido = ifelse(.data$comprometido == "S", T, F),
                  verificado = ifelse(.data$verificado == "S", T, F),
                  aprobado = ifelse(.data$aprobado == "S", T, F),
                  pagado = ifelse(.data$pagado == "S", T, F))

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_siif_comprobantes_gtos_partida_rcg01_par <- function(path){

  required_ext <- "xls"
  required_ncol <- 19
  required_title <- "Resumen de Comprobantes de Gastos (excepto REM) con detalle de partidas"
  required_nvar <- 18

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

  read_title <- stringr::str_c(db$...1[4], db$...1[5], sep = " ")

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  db <- db %>%
    dplyr::mutate(ejercicio = stringr::str_sub(...1[2], -4)) %>%
    dplyr::select(-...2, -...4, -...9) %>%
    utils::tail(-14) %>%
    dplyr::filter(...1 != is.na(...1)) %>%
    dplyr::transmute(ejercicio = .data$ejercicio,
                     nro_entrada = readr::parse_integer(...1),
                     nro_origen = readr::parse_integer(...3),
                     fuente =  ...5,
                     clase_reg =  ...6,
                     clase_gto =  ...7,
                     fecha = as.Date(readr::parse_integer(...8),
                                     origin = "1899-12-30"),
                     partida =  ...10,
                     grupo = stringr::str_c(
                       stringr::str_sub(.data$partida, 1,1), "00", ""),
                     monto = readr::parse_number(...11,
                                                 locale = readr::locale(decimal_mark = ".")),
                     cuit =  ...12,
                     beneficiario =  ...13,
                     nro_expte = ...14,
                     cta_cte =  ...15,
                     comprometido = ifelse(...16 == "S", T, F),
                     verificado = ifelse(...17 == "S", T, F),
                     aprobado = ifelse(...18 == "S", T, F),
                     pagado = ifelse(...19 == "S", T, F))

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_siif_comprobantes_gtos_gpo_partida_gto_rpa03g <- function(path){

  required_ext <- "xls"
  required_ncol <- 30
  required_title <- "DETALLE DE DOCUMENTOS ORDENADOS. PARTIDA"
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

  read_title <- stringr::str_sub(db$...18[4], 1,
                                 stringr::str_length(db$...18[4]) - 10)

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  db <- db %>%
    dplyr::mutate(ejercicio = stringr::str_sub(...18[2], -4)) %>%
    dplyr::select(.data$ejercicio, ...1, ...5, ...8, ...11,
                  ...14, ...17, ...19, ...21, ...23) %>%
    utils::tail(-20) %>%
    dplyr::filter(...1 != is.na(...1)) %>%
    dplyr::transmute(ejercicio = .data$ejercicio,
                     nro_entrada = readr::parse_integer(...1),
                     nro_origen = readr::parse_integer(...5),
                     monto = readr::parse_number(...8,
                                                 locale = readr::locale(decimal_mark = ".")),
                     mes =  readr::parse_integer(...11),
                     fecha = as.Date(readr::parse_integer(...14),
                                     origin = "1899-12-30"),
                     partida =  ...17,
                     grupo = stringr::str_c(
                       stringr::str_sub(.data$partida, 1,1), "00", ""),
                     nro_expte = ...19,
                     glose = ...21,
                     beneficiario = ...23)

  process_nvar <- ncol(db)

  # if (process_nvar != required_nvar) {
  #   abort_bad_nvar(path, process_nvar, required_nvar)
  # }

  invisible(db)

}

read_siif_retenciones_por_codigo_rao01 <- function(path){

  required_ext <- "xls"
  required_ncol <- 20
  required_title <- "Provincia de Corrientes"
  required_nvar <- 8

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

  read_title <- db$...2[2]

  if (is.na(read_title) | (read_title != required_title)) {
    abort_bad_title(path, read_title, required_title)
  }

  db <- db %>%
    utils::tail(-16) %>%
    dplyr::filter(...1 != is.na(...1)) %>%
    dplyr::transmute(fecha = as.Date(readr::parse_integer(...12),
                                     origin = "1899-12-30"),
                     ejercicio = as.character(lubridate::year(.data$fecha)),
                     nro_entrada = readr::parse_integer(...1),
                     nro_origen = readr::parse_integer(...5),
                     cod_retencion = ...4,
                     desc_retencion =  ...7,
                     monto = readr::parse_number(...11,
                                                 locale = readr::locale(decimal_mark = ".")),
                     cta_cte =  ...14)

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}
