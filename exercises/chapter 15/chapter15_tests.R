# Unit tests for chapter 15 exercises
# Load  packages
library(testthat)
library(here)

if (file.exists("exercises/helper_functions.R")) {
  source("exercises/helper_functions.R")
}

# Specify the path to your Rmd file using here::here()
rmd_file_path <- here("exercises", "chapter 14", "chapter14.Rmd")

# Run all chunks in the Rmd file to execute the exercises
all_files <- run_all_chunks(rmd_file_path)


# Exercise 1: Using pivot_longer
test_that("Using pivot_longer", {
  expect_true("measurement_type" %in% colnames(long_data))
  expect_true("value" %in% colnames(long_data))
})

# Exercise 2: Using pivot_wider
test_that("Using pivot_wider", {
  expect_true(all(c("uptake", "conc") %in% colnames(wide_data)))
})

# Exercise 3: Combined Use of pivot_longer and pivot_wider
# Original column names from CO2 dataset, except 'Plant'
original_columns <- setdiff(names(CO2), "Plant")

test_that("Combined Use of pivot_longer and pivot_wider", {
  # Test if 'Plant' column is present
  expect_true("Plant" %in% colnames(transformed_data))
  
  # Test if all other original columns are present in the transformed data
  for (col in original_columns) {
    expect_true(col %in% colnames(transformed_data))
  }
})

remove_temp_files(rmd_file_path, all_files)




