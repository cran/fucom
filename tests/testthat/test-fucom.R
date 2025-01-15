library(testthat)

# Example data from DOI org/10.3390/sym10090393
criteria_rank <- c("Criterion 1", "Criterion 2", "Criterion 3", "Criterion 4", "Criterion 5", "Criterion 6", "Criterion 7", "Criterion 8")
criteria_priority <- c(1, 1, 1, 2, 4, 4, 4, 4)

test_doi_result <- function(result) {

  # Calculating Phi based on criteria_priority from DOI org/10.3390/sym10090393
  expect_equal(
    round(as.vector(result$Phi[]), digits = 2),
    c(1, 1, 2, 2, 1, 1, 1)
  )

  # Calculating w based on criteria_priority from DOI org/10.3390/sym10090393
  expect_equal(
    round(as.vector(result$w[]), digits = 2),
    c(1, 2, 4, 2, 1, 1)
  )

  # Calculating final weights from DOI org/10.3390/sym10090393
  expect_equal(
    round(as.vector(result$weights[]), digits = 4),
    c(0.2222, 0.2222, 0.2222, 0.1110, 0.0556, 0.0556, 0.0556, 0.0556),
    tolerance = 1e-3
  )

  # Calculating DFC based on the function's internal logic and verifying it is 0 with 4 decimal places
  expect_equal(
    round(as.vector(result$DFC), digits = 4),
    0,
    tolerance = 1e-3
  )
}

test_that("Test data from DOI org/10.3390/sym10090393", {

  # Test if the criteria are in the correct format
  expect_error(fucom_method(123, c(1, 2, 3)),
               "`criteria_rank` must be a character vector and `criteria_priority` must be a numeric vector.")

  # Check if the lengths are equal
  expect_error(fucom_method(c("C1", "C2"), c(1, 2, 3)),
               "`criteria_rank` and `criteria_priority` must have the same length.")

  # Check if the DFC threshold is within the allowed range
  expect_error(fucom_method(criteria_rank, criteria_priority, DFC_threshold = -0.01),
               "`DFC_threshold` must be a positive number and less than or equal to 0.025.")

  expect_error(fucom_method(criteria_rank, criteria_priority, DFC_threshold = 0.03),
               "`DFC_threshold` must be a positive number and less than or equal to 0.025.")
})

test_that("Weight sum validation works as expected", {
  # Execute the function with the example values
  results <- fucom_method(criteria_rank, criteria_priority)

  # Test if the sum of weights equals 1
  expect_equal(sum(results$weights), 1, tolerance = 1e-3)

  # Check if all weights are greater than 0
  expect_true(all(results$weights > 0))
})

test_that("Test data from DOI org/10.3390/sym10090393 (Another Example)", {

  # Defining new criteria and priorities
  criteria_rank <- c("Criterion 1", "Criterion 2", "Criterion 3", "Criterion 4", "Criterion 5")
  criteria_priority <- c(1, 2, 2, 4, 8)

  # Execute the function with the new values
  result <- fucom_method(criteria_rank, criteria_priority)

  # Calculating Phi based on criteria_priority
  expect_equal(
    round(as.vector(result$Phi[]), digits = 2),
    c(2, 1, 2, 2)
  )

  # Calculating w based on criteria_priority
  expect_equal(
    round(as.vector(result$w[]), digits = 2),
    c(2, 2, 4)
  )

  # Calculating final weights
  expect_equal(
    round(as.vector(result$weights[]), digits = 4),
    c(0.4211, 0.2105, 0.2105, 0.1053, 0.0526),
    tolerance = 1e-3
  )

  # Calculating DFC based on the function's internal logic and verifying it is 0 with 4 decimal places
  expect_equal(
    round(as.vector(result$DFC), digits = 4),
    0,
    tolerance = 1e-3
  )
})
