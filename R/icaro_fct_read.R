read_icaro_old_obras <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 11
  required_nvar <- 11

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

  DBI::dbDisconnect(con)

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      obra = .data$Descripcion,
      imputacion = .data$Imputacion,
      partida = .data$Partida,
      fuente = .data$Fuente,
      cuit = .data$CUIT,
      cta_cte = .data$Cuenta,
      localidad = .data$Localidad,
      norma_legal = .data$NormaLegal,
      info_adicional = .data$InformacionAdicional,
      monto_contrato = .data$MontoDeContrato,
      monto_adicional = .data$Adicional
    )

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_icaro_old_carga <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 14
  required_nvar <- 14

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

  DBI::dbDisconnect(con)

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      fecha = .data$Fecha,
      nro_entrada = .data$Comprobante,
      tipo = .data$Tipo,
      obra = .data$Obra,
      imputacion = .data$Imputacion,
      partida = .data$Partida,
      fuente = .data$Fuente,
      importe = .data$Importe,
      cuit = .data$CUIT,
      cta_cte = .data$Cuenta,
      nro_certificado = .data$Certificado,
      avance = .data$Avance,
      fondo_reparo = .data$FondoDeReparo,
      origen = .data$Origen
    )

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_icaro_old_programas <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 2
  required_nvar <- 2

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

  DBI::dbDisconnect(con)

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      programa = .data$Programa,
      desc_prog = .data$DescProg
    )

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_icaro_old_subprogramas <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 3
  required_nvar <- 3

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
      programa = .data$Programa,
      subprograma = .data$Subprograma,
      desc_subprog = .data$DescSubprog
    )

  DBI::dbDisconnect(con)

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_icaro_old_proyectos <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 3
  required_nvar <- 3

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

  DBI::dbDisconnect(con)

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      subprograma = .data$Subprograma,
      proyecto = .data$Proyecto,
      desc_proy = .data$DescProy
    )

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_icaro_old_actividades <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 3
  required_nvar <- 3

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
      proyecto = .data$Proyecto,
      actividad = .data$Actividad,
      desc_act = .data$DescAct
    )

  DBI::dbDisconnect(con)

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_icaro_old_cta_cte <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 4
  required_nvar <- 4

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

  DBI::dbDisconnect(con)

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      cta_cte = .data$Cuenta,
      descripcion = .data$Descripcion,
      banco = .data$Banco,
      cta_cte_ant = .data$CuentaAnterior
    )

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_icaro_old_proveedores <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 7
  required_nvar <- 7

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

  DBI::dbDisconnect(con)

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      cuit = .data$CUIT,
      descripcion = .data$Descripcion,
      codigo = .data$Codigo,
      domicilio = .data$Domicilio,
      localidad = .data$Localidad,
      telefono = .data$Telefono,
      iva_condicion = .data$CondicionIVA
    )

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}

read_icaro_old_retenciones <- function(path){

  required_ext <- "sqlite"
  required_ncol <- 4
  required_nvar <- 4

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

  DBI::dbDisconnect(con)

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  db <- db %>%
    tibble::as_tibble() %>%
    dplyr::transmute(
      nro_entrada = .data$Comprobante,
      tipo = .data$Tipo,
      cod_ret = .data$Codigo,
      importe = .data$Importe
    )

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}


