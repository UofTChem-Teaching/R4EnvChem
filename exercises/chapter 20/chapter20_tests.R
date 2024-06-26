# Unit tests for chapter 20 exercises
# Load  packages
library(testthat)
library(here)
library(fs)

if (file.exists("exercises/helper_functions.R")) {
  source("exercises/helper_functions.R")
}

# Specify the path to your Rmd file using here::here()
rmd_file_path <- here("exercises", "chapter 20", "chapter20.Rmd")

# Exercise 2: Building Multiple Linear Regression Models
test_that("Linear Model Test", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true("TBEP" %in% models$chemical)
    expect_true(nrow(models) == 3)
    expect_true(models$intercept[1] > 5000)
    expect_true(models$slope[1] > 50000)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 3: Applying the Model for Prediction
test_that("Prediction Test", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true(nrow(sample_data) == 33)
    expect_true(!any(is.na(sample_data)))
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})
