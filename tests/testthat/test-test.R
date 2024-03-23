# Load necessary packages
library(dplyr)
library(lubridate)
# library(usethis)
library(testthat)

# print(getwd())
# print(system.file())
# raw_data <- read_delim("inst/extdata/earthquakes.tsv", delim = "\t", show_col_types = FALSE)


test_that("eq_clean_data returns a data frame", {
  # Call the eq_clean_data function
  clean_data <- eq_clean_data("earthquakes.tsv")

  # Check if the returned object is a data frame
  expect_s3_class(as.data.frame(clean_data), "data.frame")
})


test_that("eq_clean_data returns a data frame", {
  # Call the eq_clean_data function
  clean_data <- eq_location_clean(eq_clean_data("earthquakes.tsv"))

  # Check if the returned object is a data frame
  expect_s3_class(as.data.frame(clean_data), "data.frame")
})



# Mock dataset for testing
test_data <- data.frame(
  location = c("Location A", "Location B"),
  magnitude = c(5.0, 6.2),
  no_deaths = c(10, 0)
)

# Define the test case
test_that("eq_create_label creates correct HTML labels", {
  # Call the function with test data
  labels <- eq_create_label(test_data)

  # Check the output type
  expect_type(labels, "character")

  # Check the number of labels generated
  expect_length(labels, nrow(test_data))

  # Check the content of the labels
  expect_equal(labels[1], "<b>Location: </b>Location A<br><b> Magnitude: </b>5<br><b>Total deaths: </b>10")
  expect_equal(labels[2], "<b>Location: </b>Location B<br><b> Magnitude: </b>6.2<br><b>Total deaths: </b>0")
})
