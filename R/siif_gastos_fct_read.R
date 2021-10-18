#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

read_siif_ppto_gtos_fte_rf602 <- function(path){

  required_ext <- "xls"
  required_ncol <- 23
  required_title <- "DETALLE DE LA EJECUCION PRESUESTARIA"
  required_nvar <- 15

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  suppressMessages(
    db <- readxl::read_excel(path,
                             col_types = "text",
                             col_names = FALSE)
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  read_title <- (db$...1[4])
  read_title <- stringr::str_sub(read_title, 1,
                                  (stringr::str_length(read_title) - 5))

  if (read_title != required_title) {
    abort_bad_title(path, read_title, required_title)
  }

  db <- db %>%
    dplyr::transmute(ejercicio = stringr::str_sub(...1[4], -4),
                     programa = stringr::str_pad(...1, 2, pad = "0"),
                     subprograma = stringr::str_pad(...2, 2, pad = "0"),
                     proyecto = stringr::str_pad(...5, 2, pad = "0"),
                     actividad = stringr::str_pad(...6, 2, pad = "0"),
                     partida = ...7,
                     grupo = stringr::str_c(stringr::str_sub(.data$partida, 1,1), "00", ""),
                     fuente = ...8,
                     org = ...9,
                     credito_original = ...12,
                     credito_vigente = ...13,
                     comprometido = ...14,
                     ordenado = ...15,
                     saldo = ...17,
                     pendiente = ...19) %>%
    utils::tail(-13) %>%
    dplyr::filter(.data$programa != is.na(.data$programa)) %>%
    dplyr::mutate_at(c("credito_original", "credito_vigente", "comprometido",
                       "ordenado", "saldo", "pendiente"),
                     readr::parse_number,
                     locale = readr::locale(decimal_mark = "."))

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}
