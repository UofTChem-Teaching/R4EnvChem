library(testthat)

# Function to run tests in a directory
run_tests_in_directory <- function(directory) {
  # List all test files in the directory
  path <- paste("tests/", directory, sep = "")
  test_files <- list.files(path=path, pattern = "_tests\\.R$", full.names = TRUE)
  
  # Execute each test file
  for (file in test_files) {
    test_file(file)
    
  }
}

# Get all the folders inside tests folder

test_directories <- list.dirs("tests", full.names = FALSE, recursive = FALSE)


# Run tests in each directory
for (dir in test_directories) {
  cat(paste("Running tests in directory:", dir, "\n"))
  run_tests_in_directory(dir)
}


