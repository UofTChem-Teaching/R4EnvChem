# Unit tests for chapter 12 exercises

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
  
  purl(input = rmd, output = tempR)  # Extract R code from the Rmd file
  
  # Read the purled R script into a vector of code lines
  code_lines <- readLines(tempR)
  
  # Split the code into individual chunks based on the `##` comment lines (as chunk separators)
  chunk_indices <- grep("^##\\s-+\\s*$", code_lines)
  
  if (length(chunk_indices) == 0) {
    # If no chunk separators are found, run the entire code as one chunk
    tryCatch({
      eval(parse(text = code_lines), envir = envir)
    }, error = function(e) {
      message("Error in script: ", e$message)
    })
  } else {
    # Iterate through the chunk indices and execute each chunk
    for (i in seq_along(chunk_indices)) {
      # Define the start and end of each chunk
      chunk_start <- chunk_indices[i] + 1
      
      chunk_end <- if (i < length(chunk_indices)) {
        chunk_indices[i + 1] - 1
      } else {
        length(code_lines)  # Last chunk goes until the end of the file
      }
      
      # Extract the chunk lines
      chunk_lines <- code_lines[chunk_start:chunk_end]
      
      # Combine lines of the chunk into a single string
      chunk_code <- paste(chunk_lines, collapse = "\n")
      
      # Try to evaluate the chunk
      tryCatch({
        eval(parse(text = chunk_code), envir = envir)
      }, error = function(e) {
        message("Error in chunk ", i, ": ", e$message)
      })
    }
  }
}

# Function to determine the correct path for the Rmd file
get_rmd_path <- function(folder_name, filename) {
  paths <- list(
    here(filename),
    here("exercises", folder_name, filename),
    here(folder_name, filename)
  )
  for (path in paths) {
    if (file.exists(path))
      return(path)
  }
  stop("Rmd file not found!")
}

# Helper function to generate custom error messages for missing columns
missing_column_error_message <- function(data, required_columns) {
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    sprintf(
      "The dataframe does not contain the following columns: %s. Existing columns: %s",
      paste(missing_columns, collapse = ", "),
      paste(colnames(data), collapse = ", ")
    )
  } else {
    NA_character_
  }
}

# Helper function to generate custom error messages for exact column match
exact_column_match_error_message <- function(data, required_columns) {
  actual_columns <- colnames(data)
  if (!all(sort(required_columns) == sort(actual_columns))) {
    sprintf(
      "The dataframe columns do not exactly match the required columns. Required columns: %s. Existing columns: %s",
      paste(required_columns, collapse = ", "),
      paste(actual_columns, collapse = ", ")
    )
  } else {
    NA_character_
  }
}

# Specify the path to your Rmd file
rmd_file_path <- get_rmd_path("chapter 12", "chapter12.Rmd")

# Exercise 1: Ox
test_that("Ox", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    # Check if Ox_summary exists
    expect_true(exists("Ox_summary"), info = "The 'Ox_summary' object does not exist.")
    custom_error_message_columns <- missing_column_error_message(Ox_summary, c("mean_Ox", "median_Ox", "std_dev_Ox"))
    
    # Check if Ox_summary has the right columns
    expect_true(is.na(custom_error_message_columns), info = custom_error_message_columns)
    
    # Check values are numbers (not NaN or NA)
    expect_true(!is.na(Ox_summary$mean_Ox), info = "The 'mean_Ox' column contains NA or NaN values.")
    expect_true(!is.na(Ox_summary$median_Ox), info = "The 'median_Ox' column contains NA or NaN values.")
    expect_true(!is.na(Ox_summary$std_dev_Ox), info = "The 'std_dev_Ox' column contains NA or NaN values.")
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})

# Exercise 2: Make Data Long and Remove Erroneous Data
test_that("Data Cleaning Test", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Custom error message for missing columns in longer_data
    required_columns <- c("concentration", "analyte")
    custom_error_message_columns <- missing_column_error_message(longer_data, required_columns)
    
    expect_true(all(required_columns %in% colnames(longer_data)), info = custom_error_message_columns)
    # Ensure no concentration values are less than 0
    negative_concentration_rows <- longer_data$concentration < 0
    if (any(negative_concentration_rows)) {
      info_message_concentration <- paste(
        "Found",
        sum(negative_concentration_rows),
        "rows with concentration values less than 0:",
        paste(longer_data[negative_concentration_rows, ], collapse = "; ")
      )
    }
    expect_true(all(longer_data$concentration >= 0), info = info_message_concentration)
    
    # remove the temp directory
    unlink(tmp_dir, recursive = TRUE)
    
  })
  
})

# Exercise 3: The Ozone Watch
test_that("The Ozone Watch", {
  is_ordered_desc <- function(vec) {
    all(diff(vec) <= 0)
  }
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Check if all O3 values are greater than 30
    low_ozone_values <- data_high_ozone$O3 <= 30
    
    expect_true(
      all(data_high_ozone$O3 > 30),
      info = sprintf(
        "The 'O3' column in 'data_high_ozone' contains values less than or equal to 30: %s",
        paste(data_high_ozone$O3[low_ozone_values], collapse = ", ")
      )
    )
    
    expect_true(
      is_ordered_desc(data_high_ozone$Time),
      info = sprintf(
        "The 'Time' column in 'data_high_ozone' is not in descending order: %s. The first 10 values: %s",
        paste(head(data_high_ozone$Time, 10), collapse = ", "),
      )
    )
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})

# Exercise 4: High Alert Days
test_that("High Alert Days", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    # Check if the number of columns is exactly 6
    expected_col_count <- 6
    actual_col_count <- length(colnames(high_alert_data))
    expect_equal(
      actual_col_count,
      expected_col_count,
      info = sprintf(
        "Expected %d columns but found %d columns. Column names are: %s",
        expected_col_count,
        actual_col_count,
        paste(colnames(high_alert_data), collapse = ", ")
      )
    )
    
    # Check if all O3_Level values are "High"
    non_high_o3_levels <- high_alert_data$O3_Level != "High"
    
    
    expect_true(
      all(high_alert_data$O3_Level == "High"),
      info = sprintf(
        "Found %d rows where O3_Level is not 'High'. Example values: %s",
        sum(non_high_o3_levels),
        paste(unique(high_alert_data$O3_Level[non_high_o3_levels]), collapse = ", ")
      )
      
    )
    
    # remove the temp directory
    unlink(tmp_dir, recursive = TRUE)
  })
  
})

# Exercise 5: Analyzing Extreme Days
test_that("Analysis of extreme days test", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Calculate thresholds
    percentile_75_NO2 <- quantile(data$NO2, 0.75)
    avg_O3_filtered <- mean(data[data$NO2 > percentile_75_NO2, ]$O3)
    percentile_90_difference <- quantile(data[data$NO2 > percentile_75_NO2, ]$O3 - avg_O3_filtered, 0.9)
    
    # Check 1: Filtering by O3_Difference
    non_extreme_days <- extreme_days_data$O3_Difference <= percentile_90_difference
    
    expect_true(
      all(
        extreme_days_data$O3_Difference > percentile_90_difference
      ),
      info = sprintf(
        "Found %d rows where O3_Difference is not greater than the 90th percentile difference (%.2f). Example values: %s",
        sum(non_extreme_days),
        percentile_90_difference,
        paste(extreme_days_data$O3_Difference[non_extreme_days], collapse = ", ")
      )
    )
    
    
    # Check 2: Column names
    required_columns <- c("NAPS", "Time", "O3_Difference")
    custom_error_message_columns <- exact_column_match_error_message(extreme_days_data, required_columns)
    expect_true(is.na(custom_error_message_columns), info = custom_error_message_columns)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
  
})
