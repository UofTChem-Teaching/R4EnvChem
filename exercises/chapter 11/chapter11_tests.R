# Unit tests for chapter 11 exercises

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
rmd_file_path <- get_rmd_path("chapter 11", "chapter11.Rmd")

# Exercise 1: Making Data Longer
test_that("Making data longer", {
  tmp_dir <- tempdir()
  # Copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error messages for checking column names in longer_data
    custom_error_message_columns <- missing_column_error_message(longer_data, c("analyte", "concentration"))
    
    expect_true(all(c("analyte", "concentration") %in% colnames(longer_data)), info = custom_error_message_columns)
  })
  
  # Remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 2: Separate Date and Time
test_that("Separate date and time", {
  tmp_dir <- tempdir()
  # Copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for checking column names in date_time_data
    custom_error_message_columns <- missing_column_error_message(date_time_data, c("year", "month", "day", "time"))
    
    expect_true(all(
      c("year", "month", "day", "time") %in% colnames(date_time_data)
    ), info = custom_error_message_columns)
  })
  
  # Remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 3: Uniting Columns
test_that("Uniting Columns", {
  tmp_dir <- tempdir()
  # Copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for checking the 'date' column in united_data
    custom_error_message_columns <- missing_column_error_message(united_data, c("date"))
    
    expect_true("date" %in% colnames(united_data), info = custom_error_message_columns)
  })
  
  # Remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 4: Renaming Columns
test_that("Renaming columns", {
  tmp_dir <- tempdir()
  # Copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for checking the 'pollutant' column in renamed_data
    custom_error_message_columns <- missing_column_error_message(renamed_data, c("pollutant"))
    
    expect_true("pollutant" %in% colnames(renamed_data), info = custom_error_message_columns)
  })
  
  # Remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})