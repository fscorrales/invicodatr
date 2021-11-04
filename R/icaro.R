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
transmute_icaro_old_to_new <- function(path, write_csv = FALSE,
                                       write_sqlite = FALSE){

  Ans <- try_read(read_icaro_old_obras(path))

  if (write_csv == TRUE) {
    write_csv(Ans, "ICARO Obras.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("icaro", "obras",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans)

}
