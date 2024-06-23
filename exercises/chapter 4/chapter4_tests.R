#Unit tests for chapter 4 exercises

# Load testthat and here packages
library(testthat)
library(here)
library(fs)

if (file.exists("exercises/helper_functions.R")) {
  source("exercises/helper_functions.R")
}

# Specify the path to your Rmd file
rmd_file_path <- here("exercises", "chapter 4", "chapter4.Rmd")

# Test Exercise 1: Variables and Assignments
test_that("Exercise 1: Variables and Assignments", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_that(my_age, is_a("numeric"))
    expect_gte(my_age, 0)
    expect_lte(my_age, 100)
    
    expect_that(my_name, is_a("character"))
    expect_gt(length(my_name), 0)
    expect_lte(length(my_name), 50)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})

# Test Exercise 2: Basic Calculation
test_that("Exercise 2: Basic Calculation", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_equal(a, 31, tolerance = 0)
    expect_equal(b, 151.8584, tolerance = 0.0001)
    expect_equal(c, 80759608318.2073, tolerance = 0.0001)
    expect_false(c_is_integer)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})

# Test Exercise 3: Data Structures
test_that("Exercise 3: Data Structures", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_equal(my_vector, c(3, 7, 2, 9))
    expect_equal(my_names, c("Jessie", "Hyun", "Ali", "Kevin"))
    expect_true(is.list(my_list))
    expect_true(is.data.frame(my_dataframe))
    expect_equal(my_dataframe$Names, my_names)
    expect_equal(my_dataframe$Ages, my_vector)
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})




# Test Exercise 4: Calculate Median
test_that("Exercise 4: Calculate Median", {
  # function to calculate the median of a numeric vector
  med <- function(numbers) {
    sorted_numbers <- sort(numbers)
    n <- length(sorted_numbers)
    if (n %% 2 == 0) {
      return((sorted_numbers[n / 2] + sorted_numbers[n / 2 + 1]) / 2)
    } else {
      return(sorted_numbers[(n + 1) / 2])
    }
  }
  
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    expect_true(length(nums) == 10)
    expect_equal(median_value, med(nums))
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
  
})

# Test Exercise 5: Conditional Statement
test_that("Exercise 5: Conditional Statement", {
  tmp_dir <- tempdir()
  # copy contents of current directory to temp directory
  dir_copy(dirname(rmd_file_path), tmp_dir, overwrite = TRUE)
  
  withr::with_dir(tempdir(), {
    # Run all chunks in the Rmd file to execute the exercises
    suppressWarnings(run_all_chunks(rmd_file_path))
    if (grade < 50) {
      expect_equal(letter_grade, "F")
    } else if (grade >= 50 && grade <= 59) {
      expect_equal(letter_grade, "D")
    } else if (grade >= 60 && grade <= 69) {
      expect_equal(letter_grade, "C")
    } else if (grade >= 70 && grade <= 79) {
      expect_equal(letter_grade, "B")
    } else {
      expect_equal(letter_grade, "A")
    }
  })
  
  # remove the temp directory
  unlink(tmp_dir, recursive = TRUE)
})
