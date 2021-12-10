#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Transmute old ICARO DB to new version
#'
#' Returns a cleaned tibble version of old ICARO DB. Also, a csv and sqlite
#'  file could be exported.
#'
#' @param path a character vector of full path name of old ICARO DB.
#' @param write_csv logical. Should a csv file be generated?
#' @param write_sqlite logical. Should a sqlite file be generated?
#'
#' @export
rpw_slave_honorarios <- function(path, write_csv = FALSE,
                                 write_sqlite = FALSE){

  Ans <- try_read(read_slave_honorarios(path))

  if (write_csv == TRUE) {
    write_csv(Ans, "SLAVE Honorarios Factureros.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("slave", "honorarios",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}
