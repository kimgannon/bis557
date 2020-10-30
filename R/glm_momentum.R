#' @title Implement gradient descent GLM, adaptive (momentum)
#' @description This function implements gradient descent GLM using the momentum algorithm
#' @param Y a factor vector, the response variable
#' @param X a model matrix, the X variables as columns
#' @param mu_fun a function depending on Xbeta, giving the mean of the link function
#' @param lr the learning rate
#' @param gamma the constant weight of the past update (a positive number)
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
#' glm_momentum(X, y, mu_fun = function(eta) exp(eta), lr=0.00000001, max_n = 100000)

glm_momentum <- function(X, Y, mu_fun, max_n, lr, gamma=0.8, tol = 1e-10) {
  #randomly initialize beta
  beta <- matrix(rnorm(n=ncol(X)), ncol=1)

    #initialize beta_diff
    beta_diff <- c()

    #initialize update for momentum algorithm
    update <- 0

    for (i in seq_len(max_n)){
      beta_km1 <- beta
      update_km1 <- update
      eta <- X %*% beta_km1
      mu <- mu_fun(eta)
      grad <- t(X) %*% (Y-mu)

      #implement momentum as adaptive updating algorithm
      update <- lr*grad - gamma*update_km1
      beta <- beta_km1 + update

      beta_diff <- c(beta_diff, sum(beta - beta_km1)^2)
      if (tail(beta_diff,1) < tol) {
        break
      }
    }
    list(beta = beta, beta_diff = beta_diff)
  }
