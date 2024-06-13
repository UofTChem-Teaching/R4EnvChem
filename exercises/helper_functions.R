# Define the function to run all chunks from Rmd file
run_all_chunks <- function(rmd, envir = globalenv()) {
  # Get a list of all files in the directory
  all_files <- list.files(dirname(rmd))
  
  tempR <- file.path("temp.R")  # Create temporary file in the same directory
  on.exit(unlink(tempR))
  
  knitr::purl(input = rmd, output = tempR)
  
  # Source the temporary R script with chdir = TRUE so that the working directory is set to the Rmd file's directory
  source(tempR, chdir = TRUE, local = envir)
}
