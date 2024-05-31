#Unit tests for chapter 5 exercises

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
rmd_file_path <- here("exercises", "chapter 5", "chapter5.Rmd")

# Run all chunks in the Rmd file to execute the exercises
runAllChunks(rmd_file_path)

# Exercise 1: Saving R Objects

# Test Your Code (Exercise 1)

test_that("Exercise 1: Saving R Objects (CSV file)", {
  # see if the file exists in rmd_file_path
  expect_true("my_vector.csv" %in% list.files(dirname(rmd_file_path)))
})

# ------------------------------------------------------------------------

# Exercise 2: Code Troubleshooting and Readability

# Test Your Code (Exercise 2)
test_that("Exercise 2: Code Troubleshooting and Readability", {
  expect_true(result == 15)
})






