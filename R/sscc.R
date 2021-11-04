#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Read, process and write SSCC's Movimientos Banco report
#'
#' Returns a cleaned tibble version of SSCC's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @param path a character vector of full path names. If more than one path is
#'  assigned, a dataframe that combine all of them will be return.
#' @param write_csv logical. Should a csv file be generated?
#' @param write_sqlite logical. Should a sqlite file be generated?
#'
#' @export
rpw_sscc_banco_invico <- function(path, write_csv = FALSE,
                                       write_sqlite = FALSE){

  Ans <- purrr::map_df(path, ~ try_read(read_sscc_banco_invico(.x)))

  if (write_csv == TRUE) {
    write_csv(Ans, "Banco INVICO SSCC.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("sscc", "banco_invico",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}
