# Unit tests for chapter 11 exercises
# Load  packages
library(testthat)
library(here)

if (file.exists("exercises/helper_functions.R")) {
  source("exercises/helper_functions.R")
}

# Specify the path to your Rmd file using here::here()
rmd_file_path <- here("exercises", "chapter 11", "chapter11.Rmd")

# Run all chunks in the Rmd file to execute the exercises
all_files <- run_all_chunks(rmd_file_path)


# Exercise 1: Making Data Longer
test_that("Making data longer", {
  expect_true("analyte" %in% colnames(longer_data))
  expect_true("concentration" %in% colnames(longer_data))
})

# Exercise 2: Separate Date and Time
test_that("Separate date and time", {
  expect_true(all(c("year", "month", "day", "time") %in% colnames(date_time_data)))
})

# Exercise 3: Uniting Columns
test_that("Uniting Columns", {
  expect_true("date" %in% colnames(united_data))
})

# Exercise 4: Renaming Columns
test_that("Renaming columns", {
  expect_true("pollutant" %in% colnames(renamed_data))
  
})

remove_temp_files(rmd_file_path, all_files)





