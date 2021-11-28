#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Read, process and write SGF's Resumen de Rendiciones por proveedor report
#'
#' Returns a cleaned tibble version of SGF's report. Also, a csv and sqlite
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
rpw_sgf_resumen_rend_prov <- function(path, write_csv = FALSE,
                                      write_sqlite = FALSE,
                                      overwrite_sql = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_sgf_resumen_rend_prov(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Resumen Rendiciones por Proveedor SGF.csv")
  }

  if (write_sqlite == TRUE) {

    sql_db <- "sgf"
    sql_table <- "resumen_rend_prov"
    sql_key_var_1 <- "mes"
    sql_key_var_2 <- "origen"

    if (overwrite_sql == TRUE) {
      write_sqlite(sql_db, sql_table,
                   df = Ans, overwrite = TRUE)
    } else {
      filter_var <- dplyr::select(Ans, .data[[sql_key_var_1]],
                                  .data[[sql_key_var_2]]) %>%
        unique()
      execute_sqlite(sql_db,
                     paste0("DELETE FROM ", sql_table, " ",
                            "WHERE mes = ? ",
                            "AND origen = ?"),
                     params = list(filter_var[[sql_key_var_1]],
                                   filter_var[[sql_key_var_2]]))
      write_sqlite(sql_db, sql_table,
                   df = Ans, append = TRUE)
    }

  }

  invisible(Ans)

}

#' Read, process and write SGF's Listado de Proveedores report
#'
#' Returns a cleaned tibble version of SGF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_sgf_resumen_rend_prov
#' @export
rpw_sgf_listado_prov <- function(path, write_csv = FALSE,
                                      write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_sgf_listado_prov(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Listado Provedores SGF.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("sgf", "listado_prov",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}
