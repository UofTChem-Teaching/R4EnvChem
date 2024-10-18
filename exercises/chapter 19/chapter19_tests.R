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

# Specify the path to your Rmd file
rmd_file_path <- get_rmd_path("chapter 19","chapter19.Rmd")

# Exercise 2: Creating a calibration curve
test_that("Linear Model Test", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error messages for required checks
    # Custom error messages for required checks
    custom_error_message <- case_when(
      !"concentration" %in% names(coef(model)) ~
        "The 'concentration' variable is not in the model coefficients.",
      !"analyte_peak_area_counts" %in% names(model$model) ~
        "The 'analyte_peak_area_counts' variable is not in the model.",
      summary(model)$coefficients[2, 4] >= 0.05 ~
        sprintf(
          "The p-value for the slope is not less than 0.05. It is %f.",
          summary(model)$coefficients[2, 4]
        ),
      TRUE ~ NA_character_
    )
    
    # Check for required conditions in the models dataframe
    expect_true("concentration" %in% names(coef(model)), info = custom_error_message)
    expect_true("analyte_peak_area_counts" %in% names(model$model) , info = custom_error_message)
    # Test for a significant relationship
    expect_true(summary(model)$coefficients[2, 4] < 0.05 , info = custom_error_message) # P-value for the slope
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 3: Generating linear regressions for multiple analytes
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

# Exercise 4: Predicting concentrations for a new dataset
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