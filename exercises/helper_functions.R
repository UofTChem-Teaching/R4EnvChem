# Define the function to run all chunks from Rmd file
run_all_chunks <- function(rmd, envir = globalenv()) {
  
  # Get a list of all files in the directory
  all_files <- list.files(dirname(rmd))
  
  temp_dir <- dirname(rmd)  # Get the directory of the Rmd file
  tempR <- file.path(temp_dir, "temp.R")  # Create temporary file in the same directory
  on.exit(unlink(tempR))
  
  knitr::purl(input = rmd, output = tempR)
  
  # Source the temporary R script with chdir = TRUE so that the working directory is set to the Rmd file's directory
  source(tempR, chdir = TRUE)
  return (all_files)
}

# remove the files created during the test
remove_temp_files <- function(rmd_file_path , all_files) {

  #get the current files in the directory
  current_files <- list.files(dirname(rmd_file_path))
  
  #get the files that were created during the test
  new_files <- setdiff(current_files, all_files)
  
  #remove the files in the directory that were created during the test
  file.remove(file.path(dirname(rmd_file_path), new_files))
  
}
