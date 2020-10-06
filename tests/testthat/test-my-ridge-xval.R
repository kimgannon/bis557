library(testthat)

context("Test the output of homework 2, question 4")

test_that("Output gives a number, not NAN or NA", {
  library(rsample)
  irisform <- Sepal.Length~.
  d <- iris
  lambdas <- seq(0,100,by=0.1)
  flds <- rsample::vfold_cv(iris)
  opt_lambda <- my_ridge_xval(irisform, d, lambdas, flds)

  expect_false(is.na(opt_lambda) | is.nan(opt_lambda))
})

test_that("Output is non-negative", {
  library(rsample)
  irisform <- Sepal.Length~.
  d <- iris
  lambdas <- seq(0,100,by=0.1)
  flds <- rsample::vfold_cv(iris)
  opt_lambda <- my_ridge_xval(irisform, d, lambdas, flds)

  expect_true(opt_lambda >= 0)
})


