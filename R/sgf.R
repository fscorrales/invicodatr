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
#'
#' @export
rpw_sgf_resumen_rend_prov <- function(path, write_csv = FALSE,
                                  write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_sgf_resumen_rend_prov(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Resumen Rendiciones por Proveedor SGF.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("sgf", "resumen_rend_prov",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}
