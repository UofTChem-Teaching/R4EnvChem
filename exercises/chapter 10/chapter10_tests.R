# Unit tests for chapter 10 exercises
# Load  packages
library(testthat)
library(here)
library(fs)

if (file.exists("exercises/helper_functions.R")) {
  source("exercises/helper_functions.R")
}

# Specify the path to your Rmd file using here::here()
rmd_file_path <- here("exercises", "chapter 10", "chapter10.Rmd")


# Exercise 1: Importing data from CSV files
test_that("Importing Data from CSV", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true(is.data.frame(data) & nrow(data) > 0)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})


# Exercise 3: Converting Tibble to Dataframe
test_that("Converting Tibble to Dataframe", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true(exists("data") && is.data.frame(data))
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
  
  withr::with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true(file.exists("saved_data.csv"))
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
  
  withr::with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_equal(nrow(limited_data), 10)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})
