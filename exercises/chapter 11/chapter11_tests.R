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
  
  purl(input = rmd, output = tempR)  # Extract R code from the Rmd file
  
  # Read the purled R script into a vector of code lines
  code_lines <- readLines(tempR)
  
  # Split the code into individual chunks based on the `##` comment lines (as chunk separators)
  chunk_indices <- grep("^##\\s-+\\s*$", code_lines)

  if (length(chunk_indices) == 0) {
    # If no chunk separators are found, run the entire code as one chunk
    tryCatch({
      eval(parse(text = code_lines), envir = envir)
    }, error = function(e) {
      message("Error in script: ", e$message)
    })
  } else {
    # Iterate through the chunk indices and execute each chunk
    for (i in seq_along(chunk_indices)) {
      # Define the start and end of each chunk
      chunk_start <- chunk_indices[i] + 1
      
      chunk_end <- if (i < length(chunk_indices)) {
        chunk_indices[i + 1] - 1
      } else {
        length(code_lines)  # Last chunk goes until the end of the file
      }
      
      # Extract the chunk lines
      chunk_lines <- code_lines[chunk_start:chunk_end]
      
      # Combine lines of the chunk into a single string
      chunk_code <- paste(chunk_lines, collapse = "\n")
      
      # Try to evaluate the chunk
      tryCatch({
        eval(parse(text = chunk_code), envir = envir)
      }, error = function(e) {
        message("Error in chunk ", i, ": ", e$message)
      })
    }
  }
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

# Exercise 1: Separate Date and Time
test_that("Separate date and time", {
  tmp_dir <- tempdir()
  # Copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for checking column names in date_time_data
    custom_error_message_columns <- missing_column_error_message(date_time_data, c("year", "month", "day", "Hour"))
    
    expect_true(all(
      c("year", "month", "day", "Hour") %in% colnames(date_time_data)
    ), info = custom_error_message_columns)
  })
  
  # Remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 2: Uniting Columns
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

# Exercise 3: Renaming Columns
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
