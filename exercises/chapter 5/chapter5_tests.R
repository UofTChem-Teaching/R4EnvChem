#Unit tests for chapter 5 exercises

# Load packages
library(testthat)
library(here)
library(fs)
library(withr)
library(knitr)
library(tidyverse)

# Define the function to run all chunks from Rmd file
run_all_chunks <- function(rmd, envir = globalenv()) {
  tempR <- file.path("temp.R")  # Create temporary file in the same directory
  on.exit(unlink(tempR))
  
  purl(input = rmd, output = tempR)
  
  # Source the temporary R script with chdir = TRUE so that the working directory is set to the Rmd file's directory
  source(tempR, chdir = TRUE, local = envir)
  
}

# Function to determine the correct path for the Rmd file
get_rmd_path <- function(folder_name, filename) {
  paths <- list(here(filename), here("exercises", folder_name, filename), here(folder_name, filename))
  for (path in paths) {
    if (file.exists(path))
      return(path)
  }
  stop("Rmd file not found!")
}

# Specify the path to your Rmd file
rmd_file_path <- get_rmd_path("chapter 5", "chapter5.Rmd")


# Exercise 1: Saving R Objects
test_that("Exercise 1: Saving R Objects (CSV file)", {
  # see if the file exists in rmd_file_path
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true(
      file.exists("my_vector.csv"),
      info = sprintf(
        "Expected 'my_vector.csv' to be created but it does not exist. The list of files in the current directory is: %s",
        toString(list.files())
      )
    )
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})

# Exercise 2: Code Troubleshooting and Readability
test_that("Exercise 2: Code Troubleshooting and Readability", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    # Check if 'result' is equal to 15
    expect_true(result == 15,
                info = sprintf("Expected 'result' to be 15 but it is %d", result))
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})