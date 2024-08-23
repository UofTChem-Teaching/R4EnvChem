#Unit tests for chapter 7 exercises

# Load packages
library(testthat)
library(here)
library(fs)
library(withr)
library(knitr)
library(tidyverse)

# Define the function to run all chunks from Rmd file
run_all_chunks <- function(rmd, envir = globalenv()) {
  tempR <- file.path("temp.R")  # Create temporary file in the same directory
  on.exit(unlink(tempR))
  
  purl(input = rmd, output = tempR)
  
  # Source the temporary R script with chdir = TRUE so that the working directory is set to the Rmd file's directory
  source(tempR, chdir = TRUE, local = envir)
  
}

# Function to determine the correct path for the Rmd file
get_rmd_path <- function(folder_name, filename) {
  paths <- list(here(filename), here("exercises", folder_name, filename), here(folder_name, filename))
  for (path in paths) {
    if (file.exists(path))
      return(path)
  }
  stop("Rmd file not found!")
}

check_type <- function(obj) {
  if (is.list(obj) && !is.data.frame(obj)) {
    return("list")
  } else if (is.data.frame(obj)) {
    return("data.frame")
  } else if (is.matrix(obj)) {
    return("matrix")
  } else if (is.vector(obj)) {
    return(paste0("vector (", typeof(obj), ")"))
  } else if (is.factor(obj)) {
    return("factor")
  } else if (is.array(obj)) {
    return("array")
  } else {
    return(class(obj)[1])  # Returns the first class if multiple
  }
}

# Specify the path to your Rmd file
rmd_file_path <- get_rmd_path("chapter 7", "chapter7.Rmd")

#This test can be used only if it is run locally
if (rmd_file_path == here("exercises", "chapter 7", "chapter7.Rmd")) {
  # Exercise 1: Installing Packages
  test_that("Installing Packages", {
    tmp_dir <- tempdir()
    # copy contents of current directory to temp directory
    dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
    
    with_dir(tempdir(), {
      # Run all chunks in the Rmd file to execute the exercises
      suppressWarnings(run_all_chunks(rmd_file_path))
      # Task 1: Check if xgboost is installed
      expect_true("xgboost" %in% installed.packages()[, "Package"])
      
      # Task 2: Check if XML and mlr3 are installed
      packages_installed <- c("XML", "mlr3")
      expect_true(all(packages_installed %in% installed.packages()[, "Package"]))
    })
    
    # remove the temp directory
    unlink(tmp_dir, recursive = TRUE)
    
  })
}

# Exercise 2: Reading a CSV file -- tidyverse
test_that("Reading a CSV File with readr", {
  # This test expects csv_data to be defined and loaded correctly in your R session
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    # Check if csv_data is a data frame
    expect_equal(
      is.data.frame(csv_data),
      TRUE,
      info = sprintf(
        "Expected csv_data to be a data frame. It was: %s",
        check_type(csv_data)
      )
    )
    # Check if csv_data has more than 0 rows
    expect_equal(
      nrow(csv_data) > 0,
      TRUE,
      info = sprintf(
        "Expected csv_data to have more than 0 rows. It has: %d rows",
        nrow(csv_data)
      )
    )
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 3: Creating a Scatterplot -- tidyverse
test_that("Scatterplot with ggplot2", {
  # This test expects scatter_plot to be a ggplot object with at least one layer
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Check if scatter_plot is a ggplot object
    expect_equal(
      inherits(scatter_plot, "ggplot"),
      TRUE,
      info = sprintf(
        "Expected scatter_plot to be a ggplot object. Actual class: %s",
        class(scatter_plot)
      )
    )
    
    # Check if scatter_plot has at least one layer
    expect_equal(
      length(scatter_plot$layers) > 0,
      TRUE,
      info = sprintf(
        "Expected scatter_plot to have at least one layer. The number of layers is: %d",
        length(scatter_plot$layers)
      )
    )
    
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Exercise 4: Advanced filtering and summarization
test_that("Advanced filtering and summarization", {
  # This test checks the result of filtering and summarizing csv_data
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Check if the required columns are present in stats_result
    expected_columns <- c("mean_score", "sd_score")
    missing_columns <- setdiff(expected_columns, names(stats_result))
    expect_equal(
      length(missing_columns) == 0,
      TRUE,
      info = sprintf(
        "Missing columns in stats_result: %s",
        paste(missing_columns, collapse = ", ")
      )
    )
    
    # Check if stats_result has exactly 1 row
    expect_equal(
      nrow(stats_result),
      1,
      info = sprintf(
        "Expected stats_result to have 1 row. Actual number of rows: %d",
        nrow(stats_result)
      )
    )
    
    # Check if mean_score is numeric
    expect_equal(
      is.numeric(stats_result$mean_score),
      TRUE,
      info = sprintf(
        "Expected mean_score to be numeric. Actual class: %s",
        class(stats_result$mean_score)
      )
    )
    
    # Check if mean_score is in range [0, 100]
    expect_equal(
      stats_result$mean_score >= 0 &&
        stats_result$mean_score <= 100,
      TRUE,
      info = sprintf(
        "Expected mean_score to be between 0 and 100. Actual value: %s",
        toString(stats_result$mean_score)
      )
    )
    # Check if sd_score is numeric
    expect_equal(
      is.numeric(stats_result$sd_score),
      TRUE,
      info = sprintf(
        "Expected sd_score to be numeric. Actual class: %s",
        class(stats_result$sd_score)
      )
    )
    # check if sd_score is non-negative
    expect_equal(
      stats_result$sd_score >= 0,
      TRUE,
      info = sprintf(
        "Expected sd_score to be non-negative. Actual value: %s",
        toString(stats_result$sd_score)
      )
    )
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})