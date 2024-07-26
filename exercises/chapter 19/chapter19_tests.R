# Unit tests for chapter 19 exercises

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
rmd_file_path <- get_rmd_path("chapter 19", "chapter19.Rmd")

# Exercise 2: Grouping and Summarizing Data
test_that("Grouping and Summarizing Test", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for missing columns in grouped_data
    required_columns <- c("concentration", "avg_peak_area")
    custom_error_message <- missing_column_error_message(grouped_data, required_columns)
    
    # Additional custom error message for row count
    row_count_message <- if (nrow(grouped_data) <= 1) {
      sprintf(
        "The 'grouped_data' dataframe does not have more than 1 row. It has %d rows.",
        nrow(grouped_data)
      )
    } else {
      NA_character_
    }
    
    # Check for the presence of required columns in grouped_data
    expect_true(all(required_columns %in% colnames(grouped_data)), info = custom_error_message)
    # Check for the row count in grouped_data
    expect_true(nrow(grouped_data) > 1, info = row_count_message)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 3: Linear Modeling of Peak Area Relationships
test_that("Linear Model Test", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error messages for missing elements and significant relationship
    custom_error_message <- case_when(
      !"concentration" %in% names(coef(model)) ~
        sprintf(
          "The linear model does not contain the 'concentration' term in its coefficients. Coefficients: %s",
          paste(names(coef(model)), collapse = ", ")
        ),
      !"analyte_peak_area_counts" %in% names(model$model) ~
        sprintf(
          "The linear model does not contain the 'analyte_peak_area_counts' in its model data. Existing variables: %s",
          paste(names(model$model), collapse = ", ")
        ),
      summary(model)$coefficients[2, 4] >= 0.05 ~
        sprintf(
          "The p-value for the slope of the linear model is not significant. P-value: %f",
          summary(model)$coefficients[2, 4]
        ),
      TRUE ~ NA_character_
    )
    
    # Check for required elements in the model
    expect_true("concentration" %in% names(coef(model)), info = custom_error_message)
    expect_true("analyte_peak_area_counts" %in% names(model$model), info = custom_error_message)
    expect_true(summary(model)$coefficients[2, 4] < 0.05, info = custom_error_message)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 4: Advanced Data Manipulation
test_that("Advanced Data Manipulation Test", {
  tmp_dir <- tempdir()
  # Copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for missing columns in data
    required_columns <- c("peak_area_level")
    custom_error_message <- missing_column_error_message(data, required_columns)
    
    # Additional custom error message for value checks
    value_check_message <- if (any(!data$peak_area_level %in% c("Low", "Medium", "High"))) {
      sprintf(
        "The 'peak_area_level' column contains values outside the expected categories. Existing values: %s",
        paste(unique(data$peak_area_level), collapse = ", ")
      )
    } else {
      NA_character_
    }
    
    # Check if 'peak_area_level' column exists
    expect_true("peak_area_level" %in% colnames(data), info = custom_error_message)
    
    # Check if all values in 'peak_area_level' are within the specified categories
    expect_true(all(data$peak_area_level %in% c("Low", "Medium", "High")), info = value_check_message)
    
    # Custom error messages for checking non-zero counts in each category
    category_check_message <- function(category) {
      if (!(category %in% names(peak_area_counts))) {
        sprintf(
          "The 'peak_area_counts' does not contain the '%s' category. Existing categories: %s",
          category,
          paste(names(peak_area_counts), collapse = ", ")
        )
      } else {
        NA_character_
      }
    }
    
    # Check for non-zero counts in each category
    expect_true("Low" %in% names(peak_area_counts), info = category_check_message("Low"))
    expect_true("Medium" %in% names(peak_area_counts), info = category_check_message("Medium"))
    expect_true("High" %in% names(peak_area_counts), info = category_check_message("High"))
  })
  
  # Remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})