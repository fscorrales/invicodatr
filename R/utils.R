# Define global functions or variables
utils::globalVariables(paste("X", 1:60, sep = ""))

output_path <- function(){

  Ans <- stringr::str_split(getwd(), "/", simplify = T)
  Ans <- Ans[1:(length(Ans)-1)] %>%
    sapply(function(x) paste0(x, "/")) %>%
    paste(collapse = "") %>%
    paste0("R Output")

}

write_csv <- function(dt, file_name){

  file_dir <- paste0(output_path(), "/CSV Files/",
                     file_name)

  readr::write_excel_csv(dt, file_dir)

}
