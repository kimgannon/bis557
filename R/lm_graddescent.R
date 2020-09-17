#' @title Create linear model using gradient descent
#' @description This function creates a linear model using gradient descent.
#' @param f a formula the data will be fit too
#' @param d the data to be fit (a data frame)
#' @param gamma the step size (a positive number)
#' @param n number of iterations (a positive integer)
#' @param contrasts a list of variable names to be used as factor variables
#' @export
#' @examples
#' data("iris")
#' irisform <- Sepal.Length ~.
#' lm_graddescent(irisform, iris, 0.0001, 100000)

lm_graddescent <- function(f, d, gamma, n, contrasts=NULL) {
  #remove NAs in accordance with form
  d_na_rm <- stats::model.frame(f,d)
  X <- stats::model.matrix(f,d_na_rm, contrasts.arg=contrasts)
  Yn <- as.character(f)[2]
  Ymat <- matrix(d_na_rm[,Yn],ncol=1)

  #gradient is 2*t(X) %*% X - 2*t(X) %*% Ymat.
  #initial val is the zero vector
  beta <- matrix(c(rep(0,ncol(X))))
  for (i in 1:n) {
    gradient <- (2*t(X) %*% X %*% beta) - (2*t(X) %*% Ymat)
    beta <- beta - gamma*gradient
  }
  beta_names <- rownames(beta)
  beta <- as.numeric(beta)

  #handling collinearity in SVD output for computationally equivalent values
#  for (i in 1:length(beta)) {
#    if (abs(beta[i]) <= .000000001) {
#      beta[i] = NA
#    }
#  }

  names(beta) <- beta_names
  ret <- list(coefficients = beta)
  ret
}

