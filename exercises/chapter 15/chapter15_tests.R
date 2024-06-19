# Unit tests for chapter 15 exercises
# Load  packages
library(testthat)
library(here)

if (file.exists("exercises/helper_functions.R")) {
  source("exercises/helper_functions.R")
}

# Specify the path to your Rmd file using here::here()
rmd_file_path <- here("exercises", "chapter 15", "chapter15.Rmd")

# Exercise 1: Using pivot_longer
test_that("Using pivot_longer", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true("measurement_type" %in% colnames(long_data))
    expect_true("value" %in% colnames(long_data))
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 2: Using pivot_wider
test_that("Using pivot_wider", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true(all(c("uptake", "conc") %in% colnames(wide_data)))
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
  
  withr::with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    # Test if 'Plant' column is present
    expect_true("Plant" %in% colnames(transformed_data))
    
    # Test if all other original columns are present in the transformed data
    for (col in original_columns) {
      expect_true(col %in% colnames(transformed_data))
    }
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})
