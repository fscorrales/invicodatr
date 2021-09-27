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
write_sqlite <- function(sqlite_name,
                         table_name,
                         df, ...) {

  con <- connect_sqlite(sqlite_name)
  DBI::dbWriteTable(con, name = table_name, value = df, ...)
  DBI::dbDisconnect(con)

}
