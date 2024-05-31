#Unit tests for chapter 5 exercises

# Load testthat package
library(testthat)

# Define the function to run all chunks from Rmd file
runAllChunks <- function(rmd, envir = globalenv()) {
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(input = rmd, output = tempR)
  sys.source(tempR, envir = envir)
}

# Specify the path to your Rmd file
rmd_file_path <- here::here("exercises", "chapter 5", "chapter5.Rmd")

# Run all chunks in the Rmd file to execute the exercises
runAllChunks(rmd_file_path)

# Exercise 1: Saving R Objects

# Test Your Code (Exercise 1)

test_that("Exercise 1: Saving R Objects (CSV file)", {
  expect_true("my_vector.csv" %in% list.files())
})

# ------------------------------------------------------------------------

# Exercise 2: Code Troubleshooting and Readability

# Test Your Code (Exercise 2)
test_that("Exercise 2: Code Troubleshooting and Readability", {
  expect_true(result == 15)
})






