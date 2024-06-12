# Unit tests for chapter 10 exercises
# Load  packages
library(testthat)
library(here)

if (file.exists("exercises/helper_functions.R")) {
  source("exercises/helper_functions.R")
}

# Specify the path to your Rmd file using here::here()
rmd_file_path <- here("exercises", "chapter 10", "chapter10.Rmd")

# Run all chunks in the Rmd file to execute the exercises
all_files <- run_all_chunks(rmd_file_path)


# Exercise 1: Importing data from CSV files
test_that("Importing Data from CSV", {
  expect_true(is.data.frame(data) & nrow(data) > 0)
})

# Exercise 3: Converting Tibble to Dataframe
test_that("Converting Tibble to Dataframe", {
  expect_true(exists("data") && is.data.frame(data))
})

# Exercise 4: Saving Data
test_that("Saving Data to Different Formats", {
  # Check if the CSV file exists
  expect_true(file.exists(file.path(dirname(rmd_file_path), "saved_data.csv")))
})


# Exercise 5: Exploring what you can do with `read_csv()`
test_that("Exploring Input Parameters of read_csv", {
  # Check if the number of rows in the imported data is 10
  expect_equal(nrow(limited_data), 10)
})

remove_temp_files(rmd_file_path, all_files)





