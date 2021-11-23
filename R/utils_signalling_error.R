# abort(
#   "error_not_found",
#   message = "Path `blah.csv` not found",
#   path = "blah.csv"
# )
# #> Error: Path `blah.csv` not found

abort_bad_path <- function(path) {
  # msg <- glue::glue("`{path}` does not exist or is not a path")

  file_name <- div_path(path)
  file_name <- file_name[length(file_name)]

  msg <- paste0(file_name, " does not exist or supplied path is invalid")

  rlang::abort("error_bad_path",
               message = msg,
               path = path
               )

}

abort_bad_ext <- function(path, required_ext) {

  file_name <- div_path(path)
  file_name <- file_name[length(file_name)]

  read_ext <- tools::file_ext(path)

  msg <- paste0(file_name,
                " is invalid. Required file extension = ",
                required_ext)

  rlang::abort("error_bad_ext",
               message = msg,
               path = path,
               read_ext = read_ext,
               required_ext = required_ext
               )

}

abort_bad_ncol <- function(path, read_ncol, required_ncol) {

  file_name <- div_path(path)
  file_name <- file_name[length(file_name)]

  msg <- paste0(file_name, " is invalid. Required column lenght = ",
                required_ncol, " vs read column lenght = ", read_ncol)

  rlang::abort("error_bad_ncol",
               message = msg,
               path = path,
               read_ncol = read_ncol,
               required_ncol = required_ncol
  )

}

abort_bad_title <- function(path, read_title, required_title) {

  file_name <- div_path(path)
  file_name <- file_name[length(file_name)]

  msg <- paste0(file_name,
                " is invalid. Required title = ",
                required_title)

  rlang::abort("error_bad_title",
               message = msg,
               path = path,
               read_title = read_title,
               required_title = required_title
  )

}

abort_bad_nvar <- function(path, process_nvar, required_nvar) {

  file_name <- div_path(path)
  file_name <- file_name[length(file_name)]

  msg <- paste0(file_name,
                " is invalid. Required number of variables = ",
                required_nvar,
                " vs process number of variables = ",
                process_nvar)

  rlang::abort("error_bad_nvar",
               message = msg,
               path = path,
               process_nvar = process_nvar,
               required_nvar = required_nvar
  )

}

try_read <- function(my_func) {

  tryCatch(
    error_bad_path = function(cnd) {
      warning(paste0("Abort: ", conditionMessage(cnd)))
      tibble::tibble()
    },
    error_bad_ext = function(cnd) {
      warning(paste0("Abort: ", conditionMessage(cnd)))
      tibble::tibble()
    },
    error_bad_ncol = function(cnd) {
      warning(paste0("Abort: ", conditionMessage(cnd)))
      tibble::tibble()
    },
    error_bad_title = function(cnd) {
      warning(paste0("Abort: ", conditionMessage(cnd)))
      tibble::tibble()
    },
    error_bad_nvar = function(cnd) {
      warning(paste0("Abort: ", conditionMessage(cnd)))
      tibble::tibble()
    },
    error = function(cnd) {
      warning(paste0("Error: ", conditionMessage(cnd)))
      tibble::tibble()
    },
    my_func
  )

}
