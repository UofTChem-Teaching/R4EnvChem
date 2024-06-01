library(testthat)

# loading helper functions
source("exercises/helper_functions.R")

# Function to run tests in a directory
run_tests_in_directory <- function(directory) {
  # List all test files in the directory
  path <- paste("exercises/", directory, sep = "")
  test_files <- list.files(path=path, pattern = "_tests\\.R$", full.names = TRUE)
  
  # Execute each test file
  for (file in test_files) {
    test_file(file)
    
  }
}

# Get all the folders inside tests folder

test_directories <- list.dirs("exercises", full.names = FALSE, recursive = FALSE)
print(test_directories)


# Run tests in each directory
for (dir in test_directories) {
  cat(paste("Running tests in directory:", dir, "\n"))
  run_tests_in_directory(dir)
}


