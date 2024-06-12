# Unit tests for chapter 12 exercises
# Load  packages
library(testthat)
library(here)

if (file.exists("exercises/helper_functions.R")) {
  source("exercises/helper_functions.R")
}

# Specify the path to your Rmd file using here::here()
rmd_file_path <- here("exercises", "chapter 12", "chapter12.Rmd")

# Run all chunks in the Rmd file to execute the exercises
all_files <- run_all_chunks(rmd_file_path)


# Exercise 1: Ox
test_that("Ox", {
  # Check if Ox_summary exists
  expect_true(exists("Ox_summary"))
  
  # Check if Ox_summary has the right columns
  expect_equal(names(Ox_summary), c("mean_Ox", "median_Ox", "std_dev_Ox"))
  
  # Check values are numbers (not NaN or NA)
  expect_true(!is.na(Ox_summary$mean_Ox))
  expect_true(!is.na(Ox_summary$median_Ox))
  expect_true(!is.na(Ox_summary$std_dev_Ox))
  
  # Add more tests as necessary
})


# Exercise 2: Make Data Long and Remove Erroneous Data
test_that("Data Cleaning Test", {
  expect_true("analyte" %in% colnames(longer_data))
  expect_true("concentration" %in% colnames(longer_data))
  # Ensure no concentration values are less than 0
  expect_true(all(longer_data$concentration >= 0))
  
})



# Exercise 3: The Ozone Watch
is_ordered_desc <- function(vec) {
  all(diff(vec) <= 0)
}
test_that("The Ozone Watch", {
  expect_true(all(data_high_ozone$O3 > 30))
  expect_true(is_ordered_desc(data_high_ozone$Time))
})


# Exercise 4: High Alert Days
test_that("High Alert Days", {
  expect_equal(length(colnames(high_alert_data)), 6)
  expect_true(all(high_alert_data$O3_Level == "High"))
})

# Exercise 5: Analyzing Extreme Days
test_that("Analysis of extreme days test", {
  # Thresholds
  percentile_75_NO2 <- quantile(data$NO2, 0.75)
  avg_O3_filtered <- mean(data[data$NO2 > percentile_75_NO2, ]$O3)
  percentile_90_difference <- quantile(data[data$NO2 > percentile_75_NO2, ]$O3 - avg_O3_filtered, 0.9)
  
  # Check 1: Filtering by O3_Difference
  expect_true(all(extreme_days_data$O3_Difference > percentile_90_difference))
  
  # Check 2: Column names
  expected_cols <- c("NAPS", "Time", "O3_Difference")
  expect_equal(colnames(extreme_days_data), expected_cols)
  
})


remove_temp_files(rmd_file_path, all_files)




