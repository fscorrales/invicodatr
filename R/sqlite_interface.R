#' Create a connection to a SQLite database
#'
#' Connect to a SQLite database in an specif path (/R Output/SQLite Files/).
#'
#' @param sqlite_name SQLite database to connect
#'
#' @export
connect_sqlite <-function(sqlite_name){

  file_path <- paste0(output_path(), "/SQLite Files/",
                      sqlite_name, ".sqlite")

  #Use dbCanConnect() to check if a connection can be established.

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        dbname = file_path)

}
