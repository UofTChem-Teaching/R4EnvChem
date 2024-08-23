# Unit tests for chapter 20 exercises

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
rmd_file_path <- get_rmd_path("chapter 20","chapter20.Rmd")

# Exercise 2: Building Multiple Linear Regression Models
test_that("Linear Model Test", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error messages for required checks
    custom_error_message <- case_when(
      !"TBEP" %in% models$chemical ~
        sprintf(
          "The 'models' dataframe does not contain the chemical 'TBEP'. Existing chemicals: %s",
          paste(models$chemical, collapse = ", ")
        ),
      nrow(models) != 3 ~
        sprintf(
          "The 'models' dataframe does not have 3 rows. It has %d rows.",
          nrow(models)
        ),
      models$intercept[1] <= 5000 ~
        sprintf(
          "The intercept for the first model is not greater than 5000. It is %f.",
          models$intercept[1]
        ),
      models$slope[1] <= 50000 ~
        sprintf(
          "The slope for the first model is not greater than 50000. It is %f.",
          models$slope[1]
        ),
      TRUE ~ NA_character_
    )
    
    # Check for required conditions in the models dataframe
    expect_true("TBEP" %in% models$chemical, info = custom_error_message)
    expect_true(nrow(models) == 3, info = custom_error_message)
    expect_true(models$intercept[1] > 5000, info = custom_error_message)
    expect_true(models$slope[1] > 50000, info = custom_error_message)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 3: Applying the Model for Prediction
test_that("Prediction Test", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error messages for required checks
    custom_error_message <- case_when(
      nrow(sample_data) != 33 ~
        sprintf(
          "The 'sample_data' dataframe does not have 33 rows. It has %d rows.",
          nrow(sample_data)
        ),
      any(is.na(sample_data)) ~
        "The 'sample_data' dataframe contains NA values.",
      TRUE ~ NA_character_
    )
    
    # Check for required conditions in the sample_data dataframe
    expect_true(nrow(sample_data) == 33, info = custom_error_message)
    expect_true(!any(is.na(sample_data)), info = custom_error_message)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})