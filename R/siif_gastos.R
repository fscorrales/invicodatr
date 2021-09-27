#' Read, manipulate and write SIIF's rf602 report
#'
#' Returns a cleaned dataframe version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @param path a character vector of full path names. If more than one path is
#'  assigned, a dataframe that combine all of them will be return.
#' @param write_csv logical. Should a csv file be generated?
#' @param write_sqlite logical. Should a sqlite file be generated?
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
read_siif_ppto_gtos_fte <- function(path, write_csv = FALSE,
                                    write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readxl::read_excel(x,
                             col_types = "text",
                             col_names = paste("X", 1:23, sep = ""))

    db <- db %>%
      dplyr::transmute(ejecicio = stringr::str_sub(X1[4], -4),
                       programa = stringr::str_pad(X1, 2, pad = "0"),
                       subprograma = stringr::str_pad(X2, 2, pad = "0"),
                       proyecto = stringr::str_pad(X5, 2, pad = "0"),
                       actividad = stringr::str_pad(X6, 2, pad = "0"),
                       partida = X7,
                       grupo = stringr::str_c(stringr::str_sub(X7, 1,1), "00", ""),
                       fuente = X8,
                       org = X9,
                       credito_original = X12,
                       credito_vigente = X13,
                       comprometido = X14,
                       ordenado = X15,
                       saldo = X17,
                       pendiente = X19) %>%
      utils::tail(-13) %>%
      dplyr::filter(.data$programa != is.na(.data$programa)) %>%
      dplyr::mutate_at(c("credito_original", "credito_vigente", "comprometido",
                         "ordenado", "saldo", "pendiente"),
                       readr::parse_number,
                       locale = readr::locale(decimal_mark = "."))

  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Ejecucion Presupuesto por Fuente SIIF.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "ppto_gtos_fte_rf602",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}
