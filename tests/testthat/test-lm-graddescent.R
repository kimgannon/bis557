library(testthat)

context("Test the output of homework 1, question 5")

test_that("Output is similar to lm() in an easy case (same as linear_model).", {

  data("iris")
  fit_gd <- lm_graddescent(Sepal.Length ~ ., iris, 0.0001, 100000)
  fit_lm <- lm(Sepal.Length  ~ ., iris)
  expect_equivalent(fit_lm$coefficients, fit_gd$coefficients,
                    tolerance = 1e-4)
})

test_that("Function handles data with NAs and still returns similar values to lm.", {
  library(palmerpenguins)
  data("penguins")
  fit_gd <- lm_graddescent(bill_length_mm ~ species + sex, penguins, .001, 10000)
  fit_lm <- lm(bill_length_mm ~ species + sex, penguins)
  expect_equivalent(fit_lm$coefficients, fit_gd$coefficients,
                    tolerance = 1e-5)

} )

test_that("Function handles contrasts args.", {
  library(palmerpenguins)
  data("penguins")
  fit_gd <- lm_graddescent(bill_length_mm ~ species + sex, penguins, .001, 10000, contrasts=list(Species = "contr.SAS"))
  fit_lm <- lm(bill_length_mm ~ species + sex, penguins, contrasts = list(Species = "contr.SAS"))
  expect_equivalent(fit_lm$coefficients, fit_gd$coefficients,
                    tolerance = 1e-5)
})
