#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

#' Read, manipulate and write SIIF's rf602 report
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
                       grupo = stringr::str_c(stringr::str_sub(X7, 1,1), "00", ""),
                       partida = X7,
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
    write_csv(Ans, "Ejecucion Presupuesto por Fuente SIIF (rf602).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "ppto_gtos_fte_rf602",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}

#' Read, manipulate and write SIIF's rf610 report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams read_siif_ppto_gtos_fte
#' @export
read_siif_ppto_gtos_desc <- function(path, write_csv = FALSE,
                                     write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readxl::read_excel(x,
                             col_types = "text",
                             col_names = paste("X", 1:65, sep = ""))

    db <- db %>%
      dplyr::transmute(ejecicio = stringr::str_sub(X33[8], -4),
                       programa = X5,
                       subprograma = X9,
                       proyecto = X14,
                       actividad =  X17,
                       grupo = X20,
                       partida = X21,
                       desc_part = X24,
                       credito_original = X38,
                       credito_vigente = X44,
                       comprometido = X49,
                       ordenado = X55,
                       saldo = X60) %>%
      utils::tail(-28) %>%
      dplyr::mutate(programa = zoo::na.locf(.data$programa),
                    subprograma = zoo::na.locf(.data$subprograma, F),
                    proyecto = zoo::na.locf(.data$proyecto, F),
                    actividad = zoo::na.locf(.data$actividad, F),
                    grupo = zoo::na.locf(.data$grupo, F),
                    partida = zoo::na.locf(.data$partida, F),
                    desc_part = zoo::na.locf(.data$desc_part, F)) %>%
      dplyr::filter(.data$credito_original != is.na(.data$credito_original)) %>%
      dplyr::mutate_at(c("credito_original", "credito_vigente",
                         "comprometido", "ordenado", "saldo"),
                       readr::parse_number,
                       locale = readr::locale(decimal_mark = ".")) %>%
      tidyr::separate(.data$programa, c("programa", "desc_prog"),
                      remove = T, extra = "merge") %>%
      tidyr::separate(.data$subprograma, c("subprograma", "desc_subprog"),
                      remove = T, extra = "merge") %>%
      tidyr::separate(.data$proyecto, c("proyecto", "desc_proy"),
                      remove = T, extra = "merge") %>%
      tidyr::separate(.data$actividad, c("actividad", "desc_act"),
                      remove = T, extra = "merge") %>%
      tidyr::separate(.data$grupo, c("grupo", "desc_gpo"),
                      remove = T, extra = "merge") %>%
      dplyr::mutate_at(c("programa", "subprograma",
                         "proyecto", "actividad"),
                       stringr::str_pad, width = 2, pad = "0")

  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Ejecucion Presupuesto con Descripcion SIIF (rf610).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "ppto_gtos_desc_rf610",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}
#' Read, manipulate and write SIIF's rcg01_uejp report
#'
#' Returns a cleaned tibble version of SIIF's report. Also, a csv and sqlite
#'  file could be exported.
#'
#' @inheritParams read_siif_ppto_gtos_fte
#' @export
read_siif_comprobantes_gtos <- function(path, write_csv = FALSE,
                                     write_sqlite = FALSE){

  Ans <- purrr::map_df(path, function(x) {

    db <- readxl::read_excel(x,
                             col_types = "text",
                             col_names = paste("X", 1:19, sep = ""))

    db <- db %>%
      dplyr::mutate(ejericio = stringr::str_sub(X1[2], -4)) %>%
      dplyr::select(-X18, -X19)

    names(db) <- c("nro_entrada", "nro_origen", "fuente", "clase_reg",
                   "clase_mod", "clase_gto", "fecha", "monto",
                   "cuit", "beneficiario", "nro_expte","cta_cte",
                   "comprometido", "verificado", "aprobado", "pagado",
                   "nro_fondo", "ejercicio")

    db <- db %>%
      utils::tail(-15) %>%
      dplyr::filter(.data$cuit != is.na(.data$cuit)) %>%
      dplyr::filter(.data$nro_entrada != is.na(.data$nro_entrada)) %>%
      dplyr::mutate(fecha = as.Date(readr::parse_integer(.data$fecha),
                                    origin = "1899-12-30"),
                    nro_entrada = readr::parse_integer(.data$nro_entrada),
                    nro_origen = readr::parse_integer(.data$nro_origen),
                    monto = readr::parse_number(.data$monto,
                                                locale = readr::locale(decimal_mark = ".")),
             comprometido = ifelse(.data$comprometido == "S", T, F),
             verificado = ifelse(.data$verificado == "S", T, F),
             aprobado = ifelse(.data$aprobado == "S", T, F),
             pagado = ifelse(.data$pagado == "S", T, F))

  })

  if (write_csv == TRUE) {
    write_csv(Ans, "Comprobantes Gastos Ingresados SIIF (rcg01_uejp).csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("SIIF", "comprobantes_gtos_rcg01_uejp",
                 df = Ans, overwrite = TRUE)
  }

  Ans

}
