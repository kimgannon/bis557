#' @title Implement gradient descent GLM, constant
#' @description This function implements gradient descent GLM using both a constant adaptive step size
#' @param Y a factor vector, the response variable
#' @param X a model matrix, the X variables as columns
#' @param mu_fun a function depending on Xbeta, giving the mean of the link function
#' @param lr the learning rate
#' @param max_n max number of iterations (a positive integer)
#' @param tol the difference threshold for which we will exit the algorithm for iterations less than max_n
#' @export
#' @examples
#' n <- 5000; p <- 3
#' beta <- c(-1, 0.2, 0.1)
#' X <- cbind(1, matrix(rnorm(n*(p-1)), ncol = p-1))
#' eta <- X %*% beta
#' lambda <- exp(eta)
#' y <- stats::rpois(n, lambda = lambda)
#' beta_hat <- glm_gd(X, y, mu_fun = function(eta) exp(eta), lr=0.00000001, max_n = 100000)

glm_gd <- function(X, Y, mu_fun, lr, max_n=25, tol = 1e-10) {

  #randomly initialize beta
  beta <- matrix(rnorm(n=ncol(X)), ncol=1)

  #initialize beta_diff
  beta_diff <- c()

  for (i in seq_len(max_n)) {
    beta_km1 <- beta
    eta <- X %*% beta_km1
    mu <- mu_fun(eta)
    grad <- t(X) %*% (Y-mu)

    #implement update using constant step size lr
    update <- lr * grad
    beta <- beta_km1 + update

    beta_diff <- c(beta_diff, sum(beta - beta_km1)^2)
    if (tail(beta_diff,1) < tol) {
      break
    }
  }
  list(beta = beta, beta_diff = beta_diff)
}
