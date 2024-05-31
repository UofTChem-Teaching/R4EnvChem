#Unit tests for chapter 4 exercises

# Load testthat and here packages
library(testthat)
library(here)

# Define the function to run all chunks from Rmd file
runAllChunks <- function(rmd, envir = globalenv()) {
  temp_dir <- dirname(rmd)  # Get the directory of the Rmd file
  tempR <- file.path(temp_dir, "temp.R")  # Create temporary file in the same directory
  on.exit(unlink(tempR))
  
  
  knitr::purl(input = rmd, output = tempR)
  
  # Source the temporary R script with chdir = TRUE so that the working directory is set to the Rmd file's directory
  
  source(tempR, chdir = TRUE)
}



# Specify the path to your Rmd file
rmd_file_path <- here("exercises", "chapter 4", "chapter4.Rmd")

# Run all chunks in the Rmd file to execute the exercises
runAllChunks(rmd_file_path)

# Test Exercise 1: Variables and Assignments
test_that("Exercise 1: Variables and Assignments", {
  expect_that(my_age, is_a("numeric"))
  expect_gte(my_age, 0)
  expect_lte(my_age, 100)
  
  expect_that(my_name, is_a("character"))
  expect_gt(length(my_name), 0)
  expect_lte(length(my_name), 50)
})

# Test Exercise 2: Basic Calculation
test_that("Exercise 2: Basic Calculation", {
  expect_equal(a, 31, tolerance=0)
  expect_equal(b, 151.8584, tolerance=0.0001)
  expect_equal(c, 80759608318.2073, tolerance=0.0001)
  expect_false(c_is_integer)
})

# Test Exercise 3: Data Structures
test_that("Exercise 3: Data Structures", {
  expect_equal(my_vector, c(3, 7, 2, 9))
  expect_equal(my_names, c("Jessie", "Hyun", "Ali", "Kevin"))
  expect_true(is.list(my_list))
  expect_true(is.data.frame(my_dataframe))
  expect_equal(my_dataframe$Names, my_names)
  expect_equal(my_dataframe$Ages, my_vector)
})


# function to calculate the median of a numeric vector
med <- function(numbers) { 
  sorted_numbers <- sort(numbers)
  n <- length(sorted_numbers)
  if (n %% 2 == 0) {
    return((sorted_numbers[n/2] + sorted_numbers[n/2 + 1]) / 2)
  } else {
    return(sorted_numbers[(n + 1) / 2])
  }
}

# Test Exercise 4: Calculate Median
test_that("Exercise 4: Calculate Median", {
  expect_true(length(nums) == 10)
  expect_equal(median_value, med(nums))
})

# Test Exercise 5: Conditional Statement
test_that("Exercise 5: Conditional Statement", {
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

