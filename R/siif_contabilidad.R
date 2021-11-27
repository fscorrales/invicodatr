#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Read, process and write SIIF's rcocc31 report
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
rpw_siif_mayor_contable <- function(path = NULL, write_csv = FALSE,
                                    write_sqlite = FALSE,
                                    overwrite_sql = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_mayor_contable_rcocc31(.x)))

  if (nrow(Ans) != 0 ) {
    if (write_csv == TRUE) {
      write_csv(Ans, "Libro Mayor Contable SIIF (rcocc31).csv")
    }

    if (write_sqlite == TRUE) {

      sql_db <- "siif"
      sql_table <- "mayor_contable_rcocc31"
      sql_key_var_1 <- "mes"
      sql_key_var_2 <- "cta_contable"

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
                              "AND cta_contable = ?"),
                       params = list(filter_var[[sql_key_var_1]],
                                     filter_var[[sql_key_var_2]]))
        write_sqlite(sql_db, sql_table,
                     df = Ans, append = TRUE)
      }

    }

  }

  invisible(Ans)

}
