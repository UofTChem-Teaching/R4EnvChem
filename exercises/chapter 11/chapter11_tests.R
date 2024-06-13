# Unit tests for chapter 11 exercises
# Load  packages
library(testthat)
library(here)
library(fs)

if (file.exists("exercises/helper_functions.R")) {
  source("exercises/helper_functions.R")
}

# Specify the path to your Rmd file using here::here()
rmd_file_path <- here("exercises", "chapter 11", "chapter11.Rmd")

# Exercise 1: Making Data Longer
test_that("Making data longer", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  print(list.files(tmp_dir))
  withr::with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true("analyte" %in% colnames(longer_data))
    expect_true("concentration" %in% colnames(longer_data))
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})

# Exercise 2: Separate Date and Time
test_that("Separate date and time", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true(all(
      c("year", "month", "day", "time") %in% colnames(date_time_data)
    ))
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})

# Exercise 3: Uniting Columns
test_that("Uniting Columns", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  withr::with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true("date" %in% colnames(united_data))
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})

# Exercise 4: Renaming Columns
test_that("Renaming columns", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true("pollutant" %in% colnames(renamed_data))
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
  
})





