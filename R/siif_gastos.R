#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Read, manipulate and write SIIF's rf602 report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @param path a character vector of full path names. If more than one path is
#'  assigned, a dataframe that combine all of them will be return.
#' @param write_csv logical. Should a csv file be generated?
#' @param write_sqlite logical. Should a sqlite file be generated?
#'
#' @export
read_siif_ppto_gtos_fte <- function(path, write_csv = FALSE,
                                    write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readxl::read_excel(x,
                             col_types = "text",
                             col_names = FALSE)

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

  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Ejecucion Presupuesto por Fuente SIIF (rf602).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "ppto_gtos_fte_rf602",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}

#' Read, manipulate and write SIIF's rf610 report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams read_siif_ppto_gtos_fte
#' @export
read_siif_ppto_gtos_desc <- function(path, write_csv = FALSE,
                                     write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readxl::read_excel(x,
                             col_types = "text",
                             col_names = FALSE)

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

  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Ejecucion Presupuesto con Descripcion SIIF (rf610).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "ppto_gtos_desc_rf610",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}
#' Read, manipulate and write SIIF's rcg01_uejp report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams read_siif_ppto_gtos_fte
#' @export
read_siif_comprobantes_gtos <- function(path, write_csv = FALSE,
                                     write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readxl::read_excel(x,
                             col_types = "text",
                             col_names = FALSE)

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

  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Comprobantes Gastos Ingresados SIIF (rcg01_uejp).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "comprobantes_gtos_rcg01_uejp",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}

#' Read, manipulate and write SIIF's rcg01_par report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams read_siif_ppto_gtos_fte
#' @export
read_siif_comprobantes_gtos_partida <- function(path, write_csv = FALSE,
                                        write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readxl::read_excel(x,
                             col_types = "text",
                             col_names = FALSE)

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

  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Comprobantes Gastos Ingresados con Partida SIIF (rcg01_par).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "comprobantes_gtos_partida_rcg01_par",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}

#' Read, manipulate and write SIIF's gto_rpa03g report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams read_siif_ppto_gtos_fte
#' @export
read_siif_comprobantes_gtos_gpo_partida <- function(path, write_csv = FALSE,
                                                write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readxl::read_excel(x,
                             col_types = "text",
                             col_names = FALSE)

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


  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Comprobantes Gastos Ingresados por Grupo Partida SIIF (gto_rpa03g).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "comprobantes_gtos_gpo_partida_gto_rpa03g",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}


#' Read, manipulate and write SIIF's rao01 report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams read_siif_ppto_gtos_fte
#' @export
read_siif_retenciones_por_codigo <- function(path, write_csv = FALSE,
                                                    write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readxl::read_excel(x,
                             col_types = "text",
                             col_names = FALSE)

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

  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Listado Retenciones Practicada por Codigo SIIF (rao01).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "retenciones_por_codigo_rao01",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}


#' Read, manipulate and write SIIF's rfondo07tp report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams read_siif_ppto_gtos_fte
#' @export
read_siif_resumen_fdos <- function(path, write_csv = FALSE,
                                   write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readxl::read_excel(x,
                             col_types = "text",
                             col_names = FALSE)

    db <- db %>%
      dplyr::mutate(ejercicio = stringr::str_sub(...1[3], -4),
                    tipo_comprobante = stringr::str_sub(...2[10], - (length(...2[10]) - 72)))

    db <- db %>%
      utils::tail(-14) %>%
      dplyr::filter(...10 != is.na(...10)) %>%
      dplyr::transmute(tipo_comprobante = .data$tipo_comprobante,
                       ejercicio = .data$ejercicio,
                       fecha = as.Date(readr::parse_integer(...10),
                                       origin = "1899-12-30"),
                       nro_fondo = readr::parse_integer(...3),
                       glosa = ...6,
                       ingresos = readr::parse_number(...12,
                                                   locale = readr::locale(decimal_mark = ".")),
                       egresos = readr::parse_number(...15,
                                                   locale = readr::locale(decimal_mark = ".")),
                       saldo = readr::parse_number(...18,
                                                   locale = readr::locale(decimal_mark = ".")))

  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Resumen de Fondos SIIF (rfondo07tp).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "resumen_fdos_rfondo07tp",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}

