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
#' @param overwrite_sql logical. Should the sql table be overwritten? Default
#'  set to FALSE meaning that the table would be appended.
#'
#' @export
rpw_siif_ppto_gtos_fte <- function(path = NULL, write_csv = FALSE,
                                   write_sqlite = FALSE,
                                   overwrite_sql = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_ppto_gtos_fte_rf602(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Ejecucion Presupuesto por Fuente SIIF (rf602).csv")
  }

  if (write_sqlite == TRUE) {

    sql_db <- "siif"
    sql_table <- "ppto_gtos_fte_rf602"
    sql_key_var <- "ejercicio"

    if (overwrite_sql == TRUE) {
      write_sqlite(sql_db, sql_table,
                   df = Ans, overwrite = TRUE)
    } else {
      filter_var <- dplyr::select(Ans, .data[[sql_key_var]]) %>%
        unique()
      execute_sqlite(sql_db,
                     paste0("DELETE FROM ", sql_table, " ",
                            "WHERE ejercicio = ?"),
                     params = filter_var[[sql_key_var]])
      write_sqlite(sql_db, sql_table,
                   df = Ans, append = TRUE)
    }

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
    write_sqlite("siif", "ppto_gtos_desc_rf610",
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
    write_sqlite("siif", "comprobantes_gtos_rcg01_uejp",
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
rpw_siif_comprobantes_gtos_partida <- function(path, write_csv = FALSE,
                                        write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_comprobantes_gtos_partida_rcg01_par(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Comprobantes Gastos Ingresados con Partida SIIF (rcg01_par).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("siif", "comprobantes_gtos_partida_rcg01_par",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}

#' Read, process and write SIIF's gto_rpa03g report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
#' @export
rpw_siif_comprobantes_gtos_gpo_partida <- function(path, write_csv = FALSE,
                                                write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_comprobantes_gtos_gpo_partida_gto_rpa03g(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Comprobantes Gastos Ingresados por Grupo Partida SIIF (gto_rpa03g).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("siif", "comprobantes_gtos_gpo_partida_gto_rpa03g",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}


#' Read, process and write SIIF's rao01 report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
#' @export
rpw_siif_retenciones_por_codigo <- function(path, write_csv = FALSE,
                                                    write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_retenciones_por_codigo_rao01(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Listado Retenciones Practicada por Codigo SIIF (rao01).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("siif", "retenciones_por_codigo_rao01",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}


#' Read, process and write SIIF's rfondo07tp report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
#' @export
rpw_siif_resumen_fdos <- function(path, write_csv = FALSE,
                                   write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_resumen_fdos_rfondo07tp(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Resumen de Fondos SIIF (rfondo07tp).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("siif", "resumen_fdos_rfondo07tp",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}

#' Read, process and write SIIF's rdeu012 report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
#' @export
rpw_siif_deuda_flotante <- function(path, write_csv = FALSE,
                                   write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_deuda_flotante_rdeu012(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Deuda Flotante SIIF (rdeu012).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("siif", "deuda_flotante_rdeu012",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}


#' Read, process and write SIIF's rdeu012b2_c report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_ppto_gtos_fte
#' @export
rpw_siif_deuda_flotante_tg <- function(path, write_csv = FALSE,
                                     write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_deuda_flotante_tg_rdeu012b2_c(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Deuda Flotante TG SIIF (rdeu012b2_c).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("siif", "deuda_flotante_tg_rdeu012b2_c",
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
rpw_siif_pagos <- function(path, write_csv = FALSE,
                                     write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_pagos_rtr03(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Pagos SIIF (rtr03).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("siif", "pagos_rtr03",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}
