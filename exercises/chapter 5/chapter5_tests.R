#Unit tests for chapter 5 exercises

# Load testthat and here packages
library(testthat)
library(here)

if (file.exists("exercises/helper_functions.R")) {
  source("exercises/helper_functions.R")
}

# Specify the path to your Rmd file
rmd_file_path <- here("exercises", "chapter 5", "chapter5.Rmd")

# Run all chunks in the Rmd file to execute the exercises
all_files <- run_all_chunks(rmd_file_path)

# Exercise 1: Saving R Objects

# Test Your Code (Exercise 1)

test_that("Exercise 1: Saving R Objects (CSV file)", {
  # see if the file exists in rmd_file_path
  expect_true("my_vector.csv" %in% list.files(dirname(rmd_file_path)))
})

# ------------------------------------------------------------------------

# Exercise 2: Code Troubleshooting and Readability

# Test Your Code (Exercise 2)
test_that("Exercise 2: Code Troubleshooting and Readability", {
  expect_true(result == 15)
})


# Remove temporary files
remove_temp_files(rmd_file_path, all_files)


