# Unit tests for chapter 14 exercises

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

# Helper function to generate custom error messages for missing columns
missing_column_error_message <- function(data, required_columns) {
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    sprintf(
      "The dataframe does not contain the following columns: %s. Existing columns: %s",
      paste(missing_columns, collapse = ", "),
      paste(colnames(data), collapse = ", ")
    )
  } else {
    NA_character_
  }
}

# Specify the path to your Rmd file
rmd_file_path <- get_rmd_path("chapter 14", "chapter14.Rmd")

# Exercise 1: Summarizing EPDM Data
test_that("Exercise 1: Summarizing EPDM Data", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for checking the class of summary_EPDM_ft
    custom_error_message_class <- "Expected 'summary_EPDM_ft' to be of class 'flextable'."
    
    # Custom error message for checking columns in summary_EPDM
    required_columns <- c("Mean_EPDM",
                          "Median_EPDM",
                          "Min_EPDM",
                          "Max_EPDM",
                          "Std_Dev_EPDM")
    custom_error_message_columns <- missing_column_error_message(summary_EPDM, required_columns)
    
    expect_is(summary_EPDM_ft, "flextable", info = custom_error_message_class)
    expect_true(all(required_columns %in% colnames(summary_EPDM)), info = custom_error_message_columns)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 2: Grouping and Summarizing by Rounded Wavenumber
test_that("Grouping and Summarizing by Rounded Wavenumber", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for checking the class of summary_EPDM_ft
    custom_error_message_class <- "Expected 'summary_EPDM_ft' to be of class 'flextable'."
    
    # Custom error message for checking columns in summary_table
    required_columns <- c(
      "Mean_EPDM",
      "Median_EPDM",
      "Mean_Polystyrene",
      "Median_Polystyrene",
      "Mean_Polyethylene",
      "Median_Polyethylene"
    )
    custom_error_message_columns <- missing_column_error_message(summary_table, required_columns)
    
    # Check that summary_EPDM_ft is a flextable
    expect_is(summary_EPDM_ft, "flextable", info = custom_error_message_class)
    
    # Check for each column and display existing columns if missing
    expect_true(all(required_columns %in% colnames(summary_table)), info = custom_error_message_columns)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})