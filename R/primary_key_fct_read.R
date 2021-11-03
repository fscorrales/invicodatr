read_cta_cte <- function(path){

  required_ext <- "xlsx"
  required_ncol <- 9
  # required_title <- "DETALLE DE LA EJECUCION PRESUESTARIA"
  required_nvar <- 9

  if (!file.exists(path)) {
    abort_bad_path(path)
  }

  if (tools::file_ext(path) != required_ext) {
    abort_bad_ext(path, required_ext)
  }

  suppressMessages(
    db <- readxl::read_excel(path, col_types = "text",
                             sheet = "map_to_cta_cte",
                             na = "NA")
  )

  read_ncol <- ncol(db)

  if (read_ncol != required_ncol) {
    abort_bad_ncol(path, read_ncol, required_ncol)
  }

  # read_title <- (db$...1[4])
  # read_title <- stringr::str_sub(read_title, 1,
  #                                (stringr::str_length(read_title) - 5))
  #
  # if (is.na(read_title) | (read_title != required_title)) {
  #   abort_bad_title(path, read_title, required_title)
  # }

  process_nvar <- ncol(db)

  if (process_nvar != required_nvar) {
    abort_bad_nvar(path, process_nvar, required_nvar)
  }

  invisible(db)

}
