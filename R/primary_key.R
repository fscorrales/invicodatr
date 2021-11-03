#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Read, process and write a xlsx file with multiple cta_cte columns
#'
#' Returns a cleaned tibble version of the mentioned xlsx file. Also, a csv
#' and sqlite file could be exported.
#'
#' @param path a character vector of full path name.
#' @param write_csv logical. Should a csv file be generated?
#' @param write_sqlite logical. Should a sqlite file be generated?
#'
#' @export
rpw_cta_cte <- function(path, write_csv = FALSE,
                                       write_sqlite = FALSE){

  Ans <- try_read(read_cta_cte(path))

  if (write_csv == TRUE) {
    write_csv(Ans, "Cuentas Bancarias.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("primary_key", "cta_cte",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}
