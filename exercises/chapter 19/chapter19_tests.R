# Unit tests for chapter 19 exercises
# Load  packages
library(testthat)
library(here)
library(fs)

if (file.exists("exercises/helper_functions.R")) {
  source("exercises/helper_functions.R")
}

# Specify the path to your Rmd file using here::here()
rmd_file_path <- here("exercises", "chapter 19", "chapter19.Rmd")

# Exercise 2: Grouping and Summarizing Data
test_that("Grouping and Summarizing Test", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true("concentration" %in% colnames(grouped_data))
    expect_true("avg_peak_area" %in% colnames(grouped_data))
    expect_true(nrow(grouped_data) > 1)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 3: Linear Modeling of Peak Area Relationships
test_that("Linear Model Test", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true("concentration" %in% names(coef(model)))
    expect_true("analyte_peak_area_counts" %in% names(model$model))
    # Test for a significant relationship
    expect_true(summary(model)$coefficients[2, 4] < 0.05) # P-value for the slope
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 4: Advanced Data Manipulation
test_that("Advanced Data Manipulation Test", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    # Check if 'peak_area_level' column exists
    expect_true("peak_area_level" %in% colnames(data))
    
    # Check if all values in 'peak_area_level' are within the specified categories
    expect_true(all(data$peak_area_level %in% c("Low", "Medium", "High")))
    
    # Optionally, check for non-zero counts in each category (if data set is expected to cover all categories)
    expect_true(table(data$peak_area_level)[['Low']] == 4)
    expect_true(table(data$peak_area_level)[['Medium']] == 2)
    expect_true(table(data$peak_area_level)[['High']] == 2)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
  
})
