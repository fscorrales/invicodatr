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
#'
#' @export
rpw_siif_mayor_contable <- function(path = NULL, write_csv = FALSE,
                                      write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_siif_mayor_contable_rcocc31(.x)))

  if (nrow(Ans) != 0 ) {
    if (write_csv == TRUE) {
      write_csv(Ans, "Libro Mayor Contable SIIF (rcocc31).csv")
    }

    if (write_sqlite == TRUE) {
      write_sqlite("SIIF", "mayor_contable_rcocc31",
                   df = Ans, overwrite = TRUE)
    }
  }

  invisible(Ans)

}