#Unit tests for chapter 5 exercises

# Load testthat and here packages
library(testthat)
library(here)
library(fs)

if (file.exists("exercises/helper_functions.R")) {
  source("exercises/helper_functions.R")
}

# Specify the path to your Rmd file
rmd_file_path <- here("exercises", "chapter 5", "chapter5.Rmd")



# Exercise 1: Saving R Objects

# Test Your Code (Exercise 1)

test_that("Exercise 1: Saving R Objects (CSV file)", {
  # see if the file exists in rmd_file_path
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true(file.exists("my_vector.csv"))
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# ------------------------------------------------------------------------

# Exercise 2: Code Troubleshooting and Readability

# Test Your Code (Exercise 2)
test_that("Exercise 2: Code Troubleshooting and Readability", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true(result == 15)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})
