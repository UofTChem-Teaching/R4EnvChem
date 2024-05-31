#Unit tests for chapter 7 exercises

# Load testthat package
library(testthat)


# Get the original working directory
original_wd <- getwd()

# Define the function to run all chunks from Rmd file
runAllChunks <- function(rmd, envir = globalenv()) {
  temp_dir <- dirname(rmd)  # Get the directory of the Rmd file
  tempR <- file.path(temp_dir, "temp.R")  # Create temporary file in the same directory
  on.exit(unlink(tempR))
  
  # Set the working directory to the directory containing the Rmd file
  setwd(temp_dir)
  
  knitr::purl(input = rmd, output = tempR)
  
  # Source the temporary R script, which should now be able to find the CSV file
  sys.source(tempR, envir = envir)
}

# Specify the path to your Rmd file
rmd_file_path <- here::here("exercises", "chapter 7", "chpater7.Rmd")

# Run all chunks in the Rmd file to execute the exercises
runAllChunks(rmd_file_path)


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

# Change the working directory back to the original location
setwd(original_wd)


