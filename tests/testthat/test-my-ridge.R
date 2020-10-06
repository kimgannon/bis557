library(testthat)

context("Test the output of homework 2, question 3")

test_that("Output is similar to lm() in case with small lambda.", {

  data("iris")
  fit_ridge <- my_ridge(Sepal.Length ~ ., iris, 0.0001)
  fit_lm <- lm(Sepal.Length  ~ ., iris)
  expect_equivalent(fit_lm$coefficients, fit_ridge$coefficients,
                    tolerance = 1e-3)
})

test_that("Function handles data with NAs and still returns similar values to lm.", {
  library(palmerpenguins)
  data("penguins")
  fit_ridge <- my_ridge(bill_length_mm ~ species + sex, penguins, 0.02)
  fit_lm <- lm(bill_length_mm ~ species + sex, penguins)
  expect_equivalent(fit_lm$coefficients, fit_ridge$coefficients,
                    tolerance = 1e-2)

} )

