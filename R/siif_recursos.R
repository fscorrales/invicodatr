#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Read, process and write SIIF's rci02 report
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
rpw_siif_comprobantes_rec <- function(path = NULL, write_csv = FALSE,
                                      write_sqlite = FALSE,
                                      overwrite_sql = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_comprobantes_rec_rci02(.x)))

  if (nrow(Ans) != 0 ) {

    if (write_csv == TRUE) {
      write_csv(Ans, "Comprobantes Recursos Ingresados SIIF (rci02).csv")
    }

    if (write_sqlite == TRUE) {

      sql_db <- "siif"
      sql_table <- "comprobantes_rec_rci02"
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
                       params = list(filter_var[[sql_key_var]]))
        write_sqlite(sql_db, sql_table,
                     df = Ans, append = TRUE)
      }

    }

  }

  invisible(Ans)

}

#' Read, process and write SIIF's ri102 report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams rpw_siif_comprobantes_rec
#' @export

rpw_siif_ppto_rec <- function(path = NULL, write_csv = FALSE,
                                      write_sqlite = FALSE,
                                      overwrite_sql = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_ppto_rec_ri102(.x)))

  if (nrow(Ans) != 0 ) {

    if (write_csv == TRUE) {
      write_csv(Ans, "Ejecucion Presupuesto Recursos SIIF (ri102).csv")
    }

    if (write_sqlite == TRUE) {

      sql_db <- "siif"
      sql_table <- "ppto_rec_ri102"
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
                       params = list(filter_var[[sql_key_var]]))
        write_sqlite(sql_db, sql_table,
                     df = Ans, append = TRUE)
      }

    }

  }

  invisible(Ans)

}
