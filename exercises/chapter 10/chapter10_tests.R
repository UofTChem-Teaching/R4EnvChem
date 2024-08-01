# Unit tests for chapter 10 exercises

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

check_type <- function(obj) {
  if (is.list(obj) && !is.data.frame(obj)) {
    return("list")
  } else if (is.data.frame(obj)) {
    return("data.frame")
  } else if (is.matrix(obj)) {
    return("matrix")
  } else if (is.vector(obj)) {
    return(paste0("vector (", typeof(obj), ")"))
  } else if (is.factor(obj)) {
    return("factor")
  } else if (is.array(obj)) {
    return("array")
  } else {
    return(class(obj)[1])  # Returns the first class if multiple
  }
}

# Specify the path to your Rmd file
rmd_file_path <- get_rmd_path("chapter 10", "chapter10.Rmd")

# Exercise 1: Importing data from CSV files
test_that("Importing Data from CSV", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for checking if data is a data frame and has rows
    custom_error_message_data <- case_when(
      is.null(data) ~ "The 'data' object is NULL.",!is.data.frame(data) ~ sprintf(
        "The 'data' object is of type '%s' but should be a data frame.",
        check_type(data)
      ),
      nrow(data) == 0 ~ "The 'data' object is a data frame but has no rows.",
      TRUE ~ NA_character_
    )
    expect_true(!is.null(data) &&
                  is.data.frame(data) &&
                  nrow(data) > 0,
                info = custom_error_message_data)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})

# Exercise 3: Converting Tibble to Dataframe
test_that("Converting Tibble to Dataframe", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    # Custom error message for checking if converted_data is a data frame
    custom_error_message_converted_data <- case_when(
      !exists("converted_data") ~ "The 'converted_data' object does not exist.",!is.data.frame(converted_data) ~ sprintf(
        "The 'converted_data' object is of type '%s' but should be a data frame.",
        check_type(converted_data)
      ),
      TRUE ~ NA_character_
    )
    
    expect_true(exists("converted_data") &&
                  is.data.frame(converted_data),
                info = custom_error_message_converted_data)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 4: Saving Data
test_that("Saving Data to Different Formats", {
  # Check if the CSV file exists
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for checking if the CSV file exists
    custom_error_message_csv <- case_when(!file.exists("saved_data.csv") ~ {
      existing_files <- list.files(tempdir())
      sprintf(
        "The file 'saved_data.csv' does not exist in the current directory. Existing files: %s",
        paste(existing_files, collapse = ", ")
      )
    }, TRUE ~ NA_character_)
    
    expect_true(file.exists("saved_data.csv"), info = custom_error_message_csv)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})

# Exercise 5: Exploring what you can do with `read_csv()`
test_that("Exploring Input Parameters of read_csv", {
  # Check if the number of rows in the imported data is 10
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for checking the number of rows in limited_data
    custom_error_message_rows <- case_when(
      !exists("limited_data") ~ "The 'limited_data' object does not exist.",!is.data.frame(limited_data) ~ sprintf(
        "The 'limited_data' object is of type '%s' but should be a data frame.",
        check_type(limited_data)
      ),
      nrow(limited_data) != 10 ~ sprintf(
        "Expected 'limited_data' to have 10 rows but it has %d rows.",
        nrow(limited_data)
      ),
      TRUE ~ NA_character_
    )
    
    expect_true(
      exists("limited_data") &&
        is.data.frame(limited_data) &&
        nrow(limited_data) == 10,
      info = custom_error_message_rows
    )
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})