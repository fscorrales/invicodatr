% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sqlite_interface.R
\name{execute_sqlite}
\alias{execute_sqlite}
\title{Execute an update statement, query number of rows affected, and then close
result set}
\usage{
execute_sqlite(sqlite_name, sql_query, ...)
}
\arguments{
\item{sqlite_name}{A character string specifying the unquoted SQLite
database name}

\item{sql_query}{A character string containing SQL}

\item{...}{Other parameters passed on to
\code{\link[DBI]{dbGetQuery}}.}
}
\description{
Executes a statement and returns the number of rows affected.
}
