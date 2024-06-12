# Unit tests for chapter 14 exercises
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


# Exercise 1: Summarizing EPDM Data
test_that("Exercise 1: Summarizing EPDM Data", {
  expect_is(summary_EPDM_ft, "flextable")
  expect_true("Mean_EPDM" %in% colnames(summary_EPDM))
  expect_true("Median_EPDM" %in% colnames(summary_EPDM))
  expect_true("Min_EPDM" %in% colnames(summary_EPDM))
  expect_true("Max_EPDM" %in% colnames(summary_EPDM))
  expect_true("Std_Dev_EPDM" %in% colnames(summary_EPDM))
})

# Exercise 2: Grouping and Summarizing by Rounded Wavenumber
test_that("Grouping and Summarizing by Rounded Wavenumber", {
  expect_true("Mean_EPDM" %in% colnames(summary_table))
  expect_true("Median_EPDM" %in% colnames(summary_table))
  expect_true("Mean_Polystyrene" %in% colnames(summary_table))
  expect_true("Median_Polystyrene" %in% colnames(summary_table))
  expect_true("Mean_Polyethylene" %in% colnames(summary_table))
  expect_true("Median_Polyethylene" %in% colnames(summary_table))
})

remove_temp_files(rmd_file_path, all_files)




