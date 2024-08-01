# Unit tests for chapter 15 exercises

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
  paths <- list(here(filename), here("exercises", folder_name, filename))
  for (path in paths) {
    if (file.exists(path))
      return(path)
  }
  stop("Rmd file not found!")
}

# Specify the path to your Rmd file
rmd_file_path <- get_rmd_path("chapter 15", "chapter15.Rmd")

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

# Exercise 1: Using pivot_longer
test_that("Using pivot_longer", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for missing columns in long_data
    required_columns <- c("measurement_type", "value")
    custom_error_message <- missing_column_error_message(long_data, required_columns)
    
    # Check for the presence of required columns in long_data
    expect_true(all(required_columns %in% colnames(long_data)), info = custom_error_message)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 2: Using pivot_wider
test_that("Using pivot_wider", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for missing columns in wide_data
    required_columns <- c("uptake", "conc")
    custom_error_message <- missing_column_error_message(wide_data, required_columns)
    
    # Check for the presence of required columns in wide_data
    expect_true(all(required_columns %in% colnames(wide_data)), info = custom_error_message)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 3: Combined Use of pivot_longer and pivot_wider
test_that("Combined Use of pivot_longer and pivot_wider", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  # Original column names from CO2 dataset, except 'Plant'
  original_columns <- setdiff(names(CO2), "Plant")
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for missing columns in transformed_data
    required_columns <- c("Plant")
    custom_error_message <- missing_column_error_message(transformed_data, required_columns)
    
    # Check for the presence of the 'Plant' column
    expect_true("Plant" %in% colnames(transformed_data), info = custom_error_message)
    
    # Check for the presence of all original columns
    missing_columns <- setdiff(original_columns, colnames(transformed_data))
    expect_true(
      length(missing_columns) == 0,
      info = sprintf(
        "The 'transformed_data' dataframe is missing the following columns: %s. Existing columns: %s",
        paste(missing_columns, collapse = ", "),
        paste(colnames(transformed_data), collapse = ", ")
      )
    )
    
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})