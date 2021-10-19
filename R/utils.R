# Define global functions or variables
utils::globalVariables(c(paste("...", 1:70, sep = ""),
                         paste("X", 1:70, sep = ""),
                         "SQLquery"))

div_path <- function(full_path){

  Ans <- stringr::str_split(full_path, "/", simplify = T) %>%
    as.vector()

}

output_path <- function(){

  # Ans <- stringr::str_split(getwd(), "/", simplify = T)
  Ans <- div_path(getwd())
  Ans <- Ans[1:(length(Ans)-1)] %>%
    sapply(function(x) paste0(x, "/")) %>%
    paste(collapse = "") %>%
    paste0("R Output")

}

write_csv <- function(dt, file_name){

  file_full_name <- paste0(output_path(), "/CSV Files/",
                     file_name)

  # readr::write_excel_csv(dt, file_full_name)
  vroom::vroom_write(dt, file_full_name, delim = ",")

}
