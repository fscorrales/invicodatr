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

#' Copy tibble from SQLite database
#'
#' Reads an SQLite database table to a tibble from an
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

#' List field names of remote SQLite tables
#'
#' List field names of remote SQLite tables accessible through
#'  this connection from an specific path (/R Output/SQLite Files/).
#'
#' @inheritParams write_sqlite
#' @export
list_fields_sqlite <- function(sqlite_name) {

  con <- connect_sqlite(sqlite_name)
  tables <- DBI::dbListTables(con)
  Ans <- lapply(tables, function(x) DBI::dbListFields(con, x))
  DBI::dbDisconnect(con)
  names(Ans) <- tables
  return(Ans)

}

#' Sort table from an specified SQLite database
#'
#' Sort table from an specified SQLite database in an specific
#' path (/R Output/SQLite Files/)
#'
#' @param sql_sort_clause A character string containing SQL order by clause
#' @inheritParams write_sqlite
#' @export
sort_table_sqlite <- function(sqlite_name, table_name,
                              sql_sort_clause) {

  con <- connect_sqlite(sqlite_name)
  SQLquery <- paste0("CREATE TABLE COPY AS SELECT * FROM ",
                     table_name , " ",
                     "ORDER BY " , sql_sort_clause)
  DBI::dbExecute(con, SQLquery)
  SQLquery <- paste0("DROP TABLE ", table_name)
  DBI::dbExecute(con, SQLquery)
  SQLquery <- paste0("ALTER TABLE COPY RENAME TO ", table_name)
  DBI::dbExecute(con, SQLquery)
  DBI::dbDisconnect(con)

}

#' Send query, retrieve results and then clear result set
#'
#' Returns the result of a query as a tibble
#'
#' @param sql_query A character string containing SQL
#' @param ... Other parameters passed on to
#'  \code{\link[DBI]{dbGetQuery}}.
#' @inheritParams write_sqlite
#' @export
filter_sqlite <- function(sqlite_name, sql_query, ...) {

  con <- connect_sqlite(sqlite_name)
  Ans <- DBI::dbGetQuery(con, sql_query, ...)
  DBI::dbDisconnect(con)
  Ans <- tibble::as_tibble(Ans)

}


#' Execute an update statement, query number of rows affected, and then close
#'  result set
#'
#' Executes a statement and returns the number of rows affected.
#'
#' @inheritParams filter_sqlite
#' @export
execute_sqlite <- function(sqlite_name, sql_query, ...) {

  con <- connect_sqlite(sqlite_name)
  rs <- DBI::dbSendStatement(con, SQLquery, ...)
  x <- DBI::dbGetRowsAffected(rs)
  DBI::dbClearResult(rs)
  DBI::dbDisconnect(con)
  x <- paste0("Rows affected: ", x)
  print(x)

}
