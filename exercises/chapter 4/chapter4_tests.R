#Unit tests for chapter 4 exercises

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

# Specify the path to your Rmd file
rmd_file_path <- get_rmd_path("chapter 4", "chapter4.Rmd")

# Test Exercise 1: Variables and Assignments
test_that("Exercise 1: Variables and Assignments", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Test my_age variable
    expect_is(my_age,
              "numeric",
              info = paste0("my_age should be a numeric value, but it was: ", typeof(my_age)))
    expect_true(my_age >= 0,
                info = paste0("my_age should be greater than or equal to 0, but it was: ", my_age))
    expect_true(my_age <= 100,
                info = paste0("my_age should be less than or equal to 100, but it was: ", my_age))
    
    # Test my_name variable
    expect_is(
      my_name,
      "character",
      info = paste0(
        "my_name should be a character string, but it was: ",
        typeof(my_name)
      )
    )
    expect_true(
      nchar(my_name) > 0,
      info = paste0("my_name should not be an empty string, but it was empty.", my_name)
    )
    expect_true(
      nchar(my_name) <= 50,
      info = paste0(
        "my_name should be less than or equal to 50 characters, but it was: ",
        nchar(my_name)
      )
    )
    
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Test Exercise 2: Basic Calculation
test_that("Exercise 2: Basic Calculation", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Test variable a
    expect_equal(a, 31, info = paste0("Expected 'a' to be 31, but it was: ", a))
    
    # Test variable b
    expect_equal(
      b,
      151.8584,
      tolerance = 0.0001,
      info = paste0("Expected 'b' to be approximately 151.8584, but it was: ", b)
    )
    
    # Test variable c
    expect_equal(
      c,
      80759608318.2073,
      tolerance = 0.0001,
      info = paste0(
        "Expected 'c' to be approximately 80759608318.2073, but it was: ",
        c
      )
    )
    
    # Test boolean variable c_is_integer
    expect_false(c_is_integer,
                 info = paste0("Expected 'c_is_integer' to be FALSE, but it was TRUE."))
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Test Exercise 3: Data Structures
test_that("Exercise 3: Data Structures", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
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
  
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Test value of my_vector
    expected_numbers <- c(3, 7, 2, 9)
    # Custom error messages
    custom_error_message <- case_when(
      is.null(my_vector) ~
        "The 'my_vector' is NULL.",
      length(my_vector) != length(expected_numbers) ~
        sprintf(
          "Expected 'my_vector' to have length %s but it was: %s",
          length(expected_numbers),
          length(my_vector)
        ),
      any(!is.numeric(my_vector)) ~
        "Expected 'my_vector' to contain only numeric values.",
      any(is.nan(my_vector)) ~
        "The 'my_vector' contains NaN values.",
      any(is.na(my_vector)) ~
        "The 'my_vector' contains NA values.",
      !identical(my_vector, expected_numbers) ~
        sprintf(
          "Expected 'my_vector' to be %s but it was: %s",
          toString(expected_numbers),
          toString(my_vector)
        ),
      TRUE ~ NA_character_
    )
    
    expect_equal(my_vector, expected_numbers, info = custom_error_message)
    
    
    # Test value of my_names
    expected_names <- c("Jessie", "Hyun", "Ali", "Kevin")
    custom_error_message <- case_when(
      is.null(my_names) ~
        "The 'my_names' is NULL.",
      length(my_names) != length(expected_names) ~
        sprintf(
          "Expected 'my_names' to have length %s but it was: %s",
          length(expected_names),
          length(my_names)
        ),
      any(!is.character(my_names)) ~
        "Expected 'my_names' to contain only character values.",
      any(is.na(my_names)) ~
        "The 'my_names' contains NA values.",
      !identical(my_names, expected_names) ~
        sprintf(
          "Expected 'my_names' to be %s but it was: %s",
          toString(expected_names),
          toString(my_names)
        ),
      TRUE ~ NA_character_
    )
    expect_equal(my_names, expected_names, info = custom_error_message)
    
    # Test the type of my_list
    expect_true(
      is.list(my_list),
      info =
        sprintf(
          "Expected 'my_list' to be a list but it was: %s",
          check_type(my_list)
        )
    )
    
    # Test my_dataframe type and values
    expect_true(
      is.data.frame(my_dataframe),
      info = sprintf(
        "Expected 'my_dataframe' to be a data.frame but it was: %s",
        check_type(my_dataframe)
      )
    )
    expect_equal(
      my_dataframe$Names,
      my_names,
      info = sprintf(
        "The 'Names' column in 'my_dataframe' does not match 'my_names', the expected values are: %s",
        toString(expected_names),
        " but it was: ",
        toString(my_dataframe$Names)
      )
    )
    expect_equal(
      my_dataframe$Ages,
      my_vector,
      info = sprintf(
        "The 'Ages' column in 'my_dataframe' does not match 'my_vector', the expected values are: %s",
        toString(expected_numbers),
        " but it was: ",
        toString(my_dataframe$Ages)
      )
    )
    
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})


# Define the median calculation function
med <- function(numbers) {
  sorted_numbers <- sort(numbers)
  n <- length(sorted_numbers)
  if (n %% 2 == 0) {
    return((sorted_numbers[n / 2] + sorted_numbers[n / 2 + 1]) / 2)
  } else {
    return(sorted_numbers[(n + 1) / 2])
  }
}

# Test Exercise 4: Calculate Median
test_that("Exercise 4: Calculate Median", {
  tmp_dir <- tempdir()
  # Copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Test the length of 'nums'
    custom_error_message_length <- case_when(
      is.null(nums) ~ "The 'nums' vector is NULL.",
      length(nums) != 10 ~ sprintf(
        "Expected 'nums' to have length 10 but it had length: %s",
        length(nums)
      ),
      TRUE ~ NA_character_
    )
    expect_true(length(nums) == 10, info = custom_error_message_length)
    
    # Test the value of 'median_value'
    expected_median <- med(nums)
    custom_error_message_median <- case_when(
      is.null(median_value) ~ "The 'median_value' is NULL.",
      !identical(median_value, expected_median) ~ sprintf(
        "Expected 'median_value' to be %s but it was: %s",
        expected_median,
        median_value
      ),
      TRUE ~ NA_character_
    )
    expect_equal(median_value, expected_median, info = custom_error_message_median)
  })
  
  # Remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Test Exercise 5: Conditional Statement
test_that("Exercise 5: Conditional Statement", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    
    # Validate letter_grade based on grade
    custom_error_message <- case_when(
      grade < 50 && !identical(letter_grade, "F") ~
        sprintf(
          "For grade %d, expected 'letter_grade' to be 'F' but it was '%s'",
          grade,
          letter_grade
        ),
      grade >= 50 && grade <= 59 && !identical(letter_grade, "D") ~
        sprintf(
          "For grade %d, expected 'letter_grade' to be 'D' but it was '%s'",
          grade,
          letter_grade
        ),
      grade >= 60 && grade <= 69 && !identical(letter_grade, "C") ~
        sprintf(
          "For grade %d, expected 'letter_grade' to be 'C' but it was '%s'",
          grade,
          letter_grade
        ),
      grade >= 70 && grade <= 79 && !identical(letter_grade, "B") ~
        sprintf(
          "For grade %d, expected 'letter_grade' to be 'B' but it was '%s'",
          grade,
          letter_grade
        ),
      grade >= 80 && !identical(letter_grade, "A") ~
        sprintf(
          "For grade %d, expected 'letter_grade' to be 'A' but it was '%s'",
          grade,
          letter_grade
        ),
      TRUE ~ NA_character_
    )
    
    if (grade < 50) {
      expect_equal(letter_grade, "F", info = custom_error_message)
    } else if (grade >= 50 && grade <= 59) {
      expect_equal(letter_grade, "D", info = custom_error_message)
    } else if (grade >= 60 && grade <= 69) {
      expect_equal(letter_grade, "C", info = custom_error_message)
    } else if (grade >= 70 && grade <= 79) {
      expect_equal(letter_grade, "B", info = custom_error_message)
    } else {
      expect_equal(letter_grade, "A", info = custom_error_message)
    }
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})