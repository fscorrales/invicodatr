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

  ### ---INCOMPLETE--- ###

  Ans_lst <- list()

  #OBRAS
  Ans <- try_read(read_icaro_old_obras(path))

  Ans_lst$obras <- Ans

  if (write_csv == TRUE) {
    write_csv(Ans, "ICARO Obras.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("icaro_new", "obras",
                 df = Ans, overwrite = TRUE)
  }

  #CARGA
  Ans <- try_read(read_icaro_old_carga(path))

  Ans_lst$carga <- Ans

  if (write_csv == TRUE) {
    write_csv(Ans, "ICARO Carga.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("icaro_new", "carga",
                 df = Ans, overwrite = TRUE)
  }

  #PROGRAMAS
  Ans <- try_read(read_icaro_old_programas(path))

  Ans_lst$programas <- Ans

  if (write_csv == TRUE) {
    write_csv(Ans, "ICARO Programas.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("icaro_new", "programas",
                 df = Ans, overwrite = TRUE)
  }

  #SUBPROGRAMAS
  Ans <- try_read(read_icaro_old_subprogramas(path))

  Ans_lst$subprogramas <- Ans

  if (write_csv == TRUE) {
    write_csv(Ans, "ICARO Subprogramas.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("icaro_new", "subprogramas",
                 df = Ans, overwrite = TRUE)
  }

  #PROYECTOS
  Ans <- try_read(read_icaro_old_proyectos(path))

  Ans_lst$proyectos <- Ans

  if (write_csv == TRUE) {
    write_csv(Ans, "ICARO Proyectos.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("icaro_new", "proyectos",
                 df = Ans, overwrite = TRUE)
  }

  #ACTIVIDADES
  Ans <- try_read(read_icaro_old_actividades(path))

  Ans_lst$actividades <- Ans

  if (write_csv == TRUE) {
    write_csv(Ans, "ICARO Actividades.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("icaro_new", "actividades",
                 df = Ans, overwrite = TRUE)
  }

  #CUENTAS BANCARIAS
  Ans <- try_read(read_icaro_old_cta_cte(path))

  Ans_lst$ctas_ctes <- Ans

  if (write_csv == TRUE) {
    write_csv(Ans, "ICARO Cuentas Bancarias.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("icaro_new", "ctas_ctes",
                 df = Ans, overwrite = TRUE)
  }

  #PROVEEDORES
  Ans <- try_read(read_icaro_old_proveedores(path))

  Ans_lst$proveedores <- Ans

  if (write_csv == TRUE) {
    write_csv(Ans, "ICARO Proveedores.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("icaro_new", "proveedores",
                 df = Ans, overwrite = TRUE)
  }

  #RETENCIONES
  Ans <- try_read(read_icaro_old_retenciones(path))

  Ans_lst$retenciones <- Ans

  if (write_csv == TRUE) {
    write_csv(Ans, "ICARO Retenciones.csv")
  }

  if (write_sqlite == TRUE) {
    write_sqlite("icaro_new", "retenciones",
                 df = Ans, overwrite = TRUE)
  }

  invisible(Ans_lst)

}
