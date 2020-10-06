library(testthat)

context("Test the output of homework 2, question 2")

test_that("Output gives a vector", {

  data("iris")
  irisform <- Sepal.Length ~.
  fd <- rsample::vfold_cv(iris)
  os_resids_gd <- lm_graddescent_xval(irisform, iris, 0.0001, 100000, fd)

  expect_equal(class(os_resids_gd), "numeric")
})

test_that("Output does not contain NAs or NaNs", {

  data("iris")
  irisform <- Sepal.Length ~.
  fd <- rsample::vfold_cv(iris)
  os_resids_gd <- lm_graddescent_xval(irisform, iris, 0.0001, 100000, fd)

  expect_false(is.na(sum(os_resids_gd)) | is.nan(sum(os_resids_gd)))
})

test_that("Output gives nonzero vector", {

  data("iris")
  irisform <- Sepal.Length ~.
  fd <- rsample::vfold_cv(iris)
  os_resids_gd <- lm_graddescent_xval(irisform, iris, 0.0001, 100000, fd)

  expect_false(sum(abs(os_resids_gd)) == 0)
})


