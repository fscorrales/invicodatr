# Define global functions or variables
utils::globalVariables(c(paste("X", 1:70, sep = ""),
                       "SQLquery"))

output_path <- function(){

  Ans <- stringr::str_split(getwd(), "/", simplify = T)
  Ans <- Ans[1:(length(Ans)-1)] %>%
    sapply(function(x) paste0(x, "/")) %>%
    paste(collapse = "") %>%
    paste0("R Output")

}

write_csv <- function(dt, file_name){

  file_full_name <- paste0(output_path(), "/CSV Files/",
                     file_name)

  readr::write_excel_csv(dt, file_full_name)

}
