#' Create a connection to a SQLite database
#'
#' Connect to a SQLite database in an specific path (/R Output/SQLite Files/).
#'
#' @param sqlite_name SQLite database to connect
#' @param ... authentication arguments needed by the DBMS instance; these
#'  typically include user, password, host, port, dbname, etc. For details
#'  see the appropriate DBIDriver
#'
#' @export
connect_sqlite <-function(sqlite_name, ...) {

  file_path <- paste0(output_path(), "/SQLite Files/",
                      sqlite_name, ".sqlite")

  #Use dbCanConnect() to check if a connection can be established.

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = file_path, ...)

}

#' Copy data frames to SQLite database
#'
#' Writes, overwrites or appends a data frame to SQLite database in an
#'  specific path (/R Output/SQLite Files/).
#'
#' @param sqlite_name A character string specifying the unquoted SQLite
#'  database name
#' @param table_name A character string specifying the unquoted SQLite
#'  table name
#' @param df A data.frame (or coercible to data.frame)
#' @param ... Other parameters passed on to
#'  \code{\link[DBI]{dbWriteTable}}.
#'
#' @export
write_sqlite <- function(sqlite_name, table_name, df, ...) {

  con <- connect_sqlite(sqlite_name)
  DBI::dbWriteTable(con, name = table_name, value = df, ...)
  DBI::dbDisconnect(con)

}

#' Copy data frames from SQLite database
#'
#' Reads an SQLite database table to a data frames from an
#'  specific path (/R Output/SQLite Files/).
#'
#' @inheritParams write_sqlite
#' @export
read_table_sqlite <- function(sqlite_name, table_name, ...) {

  con <- connect_sqlite(sqlite_name)
  Ans <- DBI::dbReadTable(con, table_name, ...)
  DBI::dbDisconnect(con)
  Ans <- tibble::as_tibble(Ans)

}

#' List remote SQLite tables
#'
#' Returns the unquoted names of remote SQLite tables accessible through
#'  this connection from an specific path (/R Output/SQLite Files/).
#'
#' @inheritParams write_sqlite
#' @export
list_tables_sqlite <- function(sqlite_name) {

  con <- connect_sqlite(sqlite_name)
  Ans <- DBI::dbListTables(con)
  DBI::dbDisconnect(con)
  return(Ans)

}

OrdenarTablaBD <- function(DB, TablaOrdenar, strSQLOrderBy = "") {

  con <- ConectarBD(DB)
  SQLquery <- paste0("CREATE TABLE COPY AS SELECT * FROM ",
                     TablaOrdenar , " ",
                     "ORDER BY " , strSQLOrderBy)
  DBI::dbExecute(con, SQLquery)
  SQLquery <- paste0("DROP TABLE ", TablaOrdenar)
  DBI::dbExecute(con, SQLquery)
  SQLquery <- paste0("ALTER TABLE COPY RENAME TO ", TablaOrdenar)
  DBI::dbExecute(con, SQLquery)
  DesconectarBD(con)

}

FiltrarBD <- function(DB, SQLquery, params = NULL) {

  con <- ConectarBD(DB)
  Ans <- dbGetQuery(con, SQLquery, params = params)
  DesconectarBD(con)
  return(Ans)

}

EjecutarBD <- function(DB, SQLquery, params = NULL) {

  con <- ConectarBD(DB)
  dbExecute(con, SQLquery, params = params)
  DesconectarBD(con)

}


