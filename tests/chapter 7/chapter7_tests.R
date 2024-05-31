#Unit tests for chapter 7 exercises

# Load  packages
library(testthat)
library(here)

if (file.exists("tests/common/common_functions.R")) {
  source("tests/common/common_functions.R")
}


# Specify the path to your Rmd file using here::here()
rmd_file_path <- here("exercises", "chapter 7", "chpater7.Rmd")

# Run all chunks in the Rmd file to execute the exercises
all_files <- run_all_chunks(rmd_file_path)

# Exercise 1: Installing Packages
test_that("Installing Packages", {
  # Task 1: Check if xgboost is installed
  expect_true("xgboost" %in% installed.packages()[, "Package"])
  
  # Task 2: Check if XML and mlr3 are installed
  packages_installed <- c("XML", "mlr3")
  expect_true(all(packages_installed %in% installed.packages()[, "Package"]))
})

# Exercise 2: Reading a CSV file -- tidyverse
test_that("Reading a CSV File with readr", {
  # This test expects csv_data to be defined and loaded correctly in your R session
  expect_true(is.data.frame(csv_data) && nrow(csv_data) > 0)
})

# Exercise 3: Creating a Scatterplot -- tidyverse
test_that("Scatterplot with ggplot2", {
  # This test expects scatter_plot to be a ggplot object with at least one layer
  expect_true(inherits(scatter_plot, "ggplot") && length(scatter_plot$layers) > 0)
})

# Exercise 4: Advanced filtering and summarization
test_that("Advanced filtering and summarization", {
  # This test checks the result of filtering and summarizing csv_data
  expect_true(all(c("mean_score", "sd_score") %in% names(stats_result)))
  expect_equal(nrow(stats_result), 1)
  expect_true(is.numeric(stats_result$mean_score) && stats_result$mean_score >= 0 && stats_result$mean_score <= 100)
  expect_true(is.numeric(stats_result$sd_score) && stats_result$sd_score >= 0)
})

remove_temp_files(rmd_file_path, all_files)

