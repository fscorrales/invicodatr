#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Read, process and write SIIF's rf602 report
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
rpw_siif_ppto_gtos_fte <- function(path = NULL, write_csv = FALSE,
                                    write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_ppto_gtos_fte_rf602(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Ejecucion Presupuesto por Fuente SIIF (rf602).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "ppto_gtos_fte_rf602",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}

#' Read, process and write SIIF's rf610 report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
#' @export
rpw_siif_ppto_gtos_desc <- function(path, write_csv = FALSE,
                                     write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_ppto_gtos_desc_rf610(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Ejecucion Presupuesto con Descripcion SIIF (rf610).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "ppto_gtos_desc_rf610",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}
#' Read, process and write SIIF's rcg01_uejp report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
#' @export
rpw_siif_comprobantes_gtos <- function(path, write_csv = FALSE,
                                     write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_comprobantes_gtos_rcg01_uejp(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Comprobantes Gastos Ingresados SIIF (rcg01_uejp).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "comprobantes_gtos_rcg01_uejp",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}

#' Read, process and write SIIF's rcg01_par report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
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

#' Read, process and write SIIF's gto_rpa03g report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
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


#' Read, process and write SIIF's rao01 report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
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


#' Read, process and write SIIF's rfondo07tp report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
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

#' Read, process and write SIIF's rdeu012 report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
#' @export
read_siif_deuda_flotante <- function(path, write_csv = FALSE,
                                   write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readxl::read_excel(x,
                             col_types = "text",
                             col_names = FALSE)

    db <- db %>%
      dplyr::mutate(fuente = ifelse(...5 == "27", NA, as.numeric(...5)),
                    fecha_desde = stringr::str_sub(...1[14], 7 ,17),
                    fecha_hasta = stringr::str_sub(...1[14], -10)) %>%
      utils::tail(-11) %>%
      dplyr::transmute(fuente = zoo::na.locf(.data$fuente),
                       fecha_desde = lubridate::dmy(.data$fecha_desde),
                       fecha_hasta = lubridate::dmy(.data$fecha_hasta),
                       mes_hasta = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha_hasta),
                                                                   2, pad = "0"),
                                                  lubridate::year(.data$fecha_hasta), sep = "/"),
                       nro_entrada = ...1,
                       nro_origen = ...3,
                       fecha_aprobado = ...6,
                       org_fin = ...8,
                       monto = ...9,
                       saldo = ...12,
                       nro_expte = ...13,
                       cta_cte = ...14,
                       referencia = ...16,
                       cuit = ...17,
                       beneficiario = ...18) %>%
      dplyr::filter(!is.na(.data$cuit),
                    !is.na(.data$nro_entrada)) %>%
      dplyr::mutate(fecha_aprobado = as.Date(readr::parse_integer(.data$fecha_aprobado),
                                             origin = "1899-12-30"),
                    nro_entrada = readr::parse_integer(.data$nro_entrada),
                    nro_origen = readr::parse_integer(.data$nro_origen),
                    monto = round(readr::parse_double(.data$monto), 2),
                    saldo = round(readr::parse_double(.data$saldo), 2))

  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Deuda Flotante SIIF (rdeu012).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "deuda_flotante_rdeu012",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}


#' Read, process and write SIIF's rdeu012b2_c report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
#' @export
read_siif_deuda_flotante_tg <- function(path, write_csv = FALSE,
                                     write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readr::read_csv(x, col_names = FALSE,
                          col_types = readr::cols(.default = "c"),
                          locale = readr::locale(encoding = stringi::stri_enc_get()))

    db <- db %>%
      dplyr::mutate(fecha_desde = stringr::str_sub(X1[7], 7 ,17),
                    fecha_hasta = stringr::str_sub(X1[7], -10)) %>%
      dplyr::filter(!is.na(X6)) %>%
      utils::head(-2) %>%
      dplyr::mutate(ejercicio = ifelse(is.na(X1), stringr::str_sub(X2[], -4), NA)) %>%
      dplyr::transmute(ejercicio = zoo::na.locf(.data$ejercicio, fromLast = TRUE),
                       fuente = X3,
                       fecha_desde = lubridate::dmy(.data$fecha_desde),
                       fecha_hasta = lubridate::dmy(.data$fecha_hasta),
                       mes_hasta = stringr::str_c(stringr::str_pad(lubridate::month(.data$fecha_hasta),
                                                                   2, pad = "0"),
                                                  lubridate::year(.data$fecha_hasta), sep = "/"),
                       nro_entrada = X1,
                       nro_origen = X2,
                       cc = stringr::str_c(.data$nro_entrada,
                                           stringr::str_sub(.data$ejercicio, -2), sep = "/"),
                       org_fin = X4,
                       monto = X5,
                       saldo = X6,
                       nro_expte = X7,
                       cta_cte = X8,
                       glosa = X9) %>%
      dplyr::filter(!is.na(.data$fuente),
                    .data$fuente != "Fte") %>%
      dplyr::mutate(nro_entrada = readr::parse_integer(.data$nro_entrada),
                    nro_origen = readr::parse_integer(.data$nro_origen)) %>%
      dplyr::mutate_at(c("monto", "saldo"),
                       ~round(readr::parse_number(.,
                                                  locale = readr::locale(decimal_mark = ","))))

  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Deuda Flotante TG SIIF (rdeu012b2_c).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "deuda_flotante_tg_rdeu012b2_c",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}

#' Read, process and write SIIF's rt03 report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
#' @export
read_siif_pagos <- function(path, write_csv = FALSE,
                                     write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readxl::read_excel(x,
                             col_types = "text",
                             col_names = FALSE)

    db <- db %>%
      utils::tail(-18) %>%
      dplyr::mutate_all(stringr::str_replace_all,
                        pattern = "[\r\n]", replacement = "") %>%
      dplyr::transmute(ejercicio = ...9,
                       entidad = ...6,
                       nro_entrada = ...8,
                       ejercicio_origen = ...1,
                       nro_cap = ...10,
                       fuente = ...12,
                       fecha_pago = ...13,
                       cta_cte_pago = ...16,
                       nro_interno = ...18,
                       chq_bco = ...19,
                       tipo = ...21,
                       cuit_pago = ...23,
                       beneficiario = ...25,
                       glosa = ...27,
                       monto = ...29,
                       ingresos = ...38,
                       uej = ...42) %>%
      dplyr::filter(!is.na(.data$ejercicio_origen)) %>%
      dplyr::mutate(fecha_pago = as.Date(readr::parse_integer(.data$fecha_pago),
                                         origin = "1899-12-30"),
                    monto = round(readr::parse_double(.data$monto), 2),
                    ingresos = round(readr::parse_double(.data$ingresos), 2))

    #Deleting some cols
    db <- db %>%
      dplyr::select(-.data$ingresos, -.data$uej, -.data$chq_bco,
                    -.data$nro_interno, -.data$ejercicio_origen, -.data$entidad)

    #ERROR con los ANP (el reporte solo trae un CAP por lo que el mov. queda anulado)
    db <- db %>%
      dplyr::filter(.data$tipo != "ANP")

    # #Transformamos los ANP en CAP
    # BD <- BD %>%
    #   mutate(Tipo = ifelse(Tipo == "ANP", "CAP", Tipo))

    #Generamos un proxy del Cta Cte Gasto
    carga_gasto <- db %>%
      dplyr::filter(!.data$cuit_pago %in% c(30709110078, 33693450239)) %>%
      dplyr::group_by(.data$nro_entrada, .data$tipo) %>%
      dplyr::filter(.data$monto == max(.data$monto)) %>%
      dplyr::select(.data$nro_entrada, .data$tipo,
                    cta_cte = .data$cta_cte_pago, cuit = .data$cuit_pago) %>%
      unique()

    db <- db %>%
      dplyr::left_join(carga_gasto) %>%
      dplyr::mutate(cta_cte = dplyr::if_else(is.na(.data$cta_cte),
                                             .data$cta_cte_pago, .data$cta_cte),
                    cuit = dplyr::if_else(is.na(.data$cuit),
                                          .data$cuit_pago, .data$cuit))

    # #Volvemos a crear el Tipo ANP
    # BD <- BD %>%
    #   mutate(Tipo = ifelse(Monto < 0, "ANP", Tipo))

    db <- db %>%
      dplyr::select(.data$ejercicio, .data$nro_entrada, .data$tipo,
                    .data$fuente, .data$fecha_pago, .data$cta_cte,
                    .data$cuit, .data$cta_cte_pago, .data$cuit_pago,
                    .data$beneficiario, .data$monto, dplyr::everything())

  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Pagos SIIF (rtr03).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "pagos_rtr03",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}
