library(testthat)

context("Test the output of homework 3, question 2")

test_that("Momentum adaptive GD is similar to true betas", {

  n <- 5000; p <- 3
  beta <- c(-1, 0.2, 0.1)
  X <- cbind(1, matrix(rnorm(n*(p-1)), ncol = p-1))
  eta <- X %*% beta
  lambda <- exp(eta)
  y <- stats::rpois(n, lambda = lambda)
  momentum <- glm_momentum(X, y, mu_fun = function(eta) exp(eta), lr=0.00000001, max_n = 100000)
  expect_equivalent(momentum$beta, beta,
                    tolerance = 2)
})

test_that("Constant step size GD returns value similar to true betas", {
  n <- 5000; p <- 3
  beta <- c(-1, 0.2, 0.1)
  X <- cbind(1, matrix(rnorm(n*(p-1)), ncol = p-1))
  eta <- X %*% beta
  lambda <- exp(eta)
  y <- stats::rpois(n, lambda = lambda)
  gd <- glm_gd(X, y, mu_fun = function(eta) exp(eta), lr=0.00000001, max_n = 100000)
  expect_equivalent(gd$beta, beta,
                    tolerance = 2)

} )
