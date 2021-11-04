read_invico_old_obras <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 11

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = path)

  suppressMessages(
    db <- DBI::dbReadTable(con, "OBRAS")
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      obra = Descripcion,
      imputacion = Imputacion,
      partida = Partida,
      fuente = Fuente,
      cuit = CUIT,
      cta_cte = Cuenta,
      localidad = Localidad,
      norma_legal = NormaLegal,
      info_adicional = InformacionAdicional,
      monto_contrato = MontoDeContrato,
      monto_adicional = Adicional
    )

  DBI::dbDisconnect(con)

  invisible(db)

}

read_invico_old_carga <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 14

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = path)

  suppressMessages(
    db <- DBI::dbReadTable(con, "CARGA")
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      fecha = Fecha,
      nro_entrada = Comprobante,
      tipo = Tipo,
      obra = Obra,
      imputacion = Imputacion,
      partida = Partida,
      fuente = Fuente,
      importe = Importe,
      cuit = CUIT,
      cta_cte = Cuenta,
      nro_certificado = Certificado,
      avance = Avance,
      fondo_reparo = FondoDeReparo,
      origen = Origen
    )

  DBI::dbDisconnect(con)

  invisible(db)

}

read_invico_old_programas <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 2

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = path)

  suppressMessages(
    db <- DBI::dbReadTable(con, "PROGRAMAS")
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      programa = Programa,
      desc_prog = DescProg
    )

  DBI::dbDisconnect(con)

  invisible(db)

}

read_invico_old_subprogramas <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 3

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = path)

  suppressMessages(
    db <- DBI::dbReadTable(con, "SUBPROGRAMAS")
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      programa = Programa,
      subprograma = Subprograma,
      desc_subprog = DescSubprog
    )

  DBI::dbDisconnect(con)

  invisible(db)

}

read_invico_old_proyectos <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 3

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = path)

  suppressMessages(
    db <- DBI::dbReadTable(con, "PROYECTOS")
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      subprograma = Subprograma,
      proyecto = Proyecto,
      desc_proy = DescProy
    )

  DBI::dbDisconnect(con)

  invisible(db)

}

read_invico_old_actividades <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 3

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = path)

  suppressMessages(
    db <- DBI::dbReadTable(con, "ACTIVIDADES")
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      proyecto = Proyecto,
      actividad = Actividad,
      desc_act = DescAct
    )

  DBI::dbDisconnect(con)

  invisible(db)

}

read_invico_old_cta_cte <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 4

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = path)

  suppressMessages(
    db <- DBI::dbReadTable(con, "CUENTASBANCARIAS")
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      cta_cte = Cuenta,
      descripcion = Descripcion,
      banco = Banco,
      cta_cte_ant = CuentaAnterior
    )

  DBI::dbDisconnect(con)

  invisible(db)

}

read_invico_old_proveedores <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 7

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = path)

  suppressMessages(
    db <- DBI::dbReadTable(con, "PROVEEDORES")
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      cuit = CUIT,
      descripcion = Descripcion,
      codigo = Codigo,
      domicilio = Domicilio,
      localidad = Localidad,
      telefono = Telefono,
      iva_condicion = CondicionIVA
    )

  DBI::dbDisconnect(con)

  invisible(db)

}

read_invico_old_retenciones <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 4

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = path)

  suppressMessages(
    db <- DBI::dbReadTable(con, "RETENCIONES")
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      nro_entrada = Comprobante,
      tipo = Tipo,
      cod_ret = Codigo,
      importe = Importe
    )

  DBI::dbDisconnect(con)

  invisible(db)

}
