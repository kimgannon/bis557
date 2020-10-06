#' @title Ridge regression for collinear/near-collinear columns
#' @description This function fits a ridge regression model, accounting for collinear/near collinear columns.
#' @param f a formula the data will be fit too
#' @param lambda the adjustment/"penalty" parameter
#' @export
#' @examples
#' data("iris")
#' irisform <- Sepal.Length ~.
#' my_ridge(irisform, iris, .02)

my_ridge <- function(f, d, lambda) {
  mms <- make_model_matrices (f, d)
  X <- mms$X
  Y <- mms$Y
  svd_x <- svd(X)
  Sigma <- diag(svd_x$d)
  lambda_I <- diag(rep(lambda, length(svd_x$d)))
  beta <- svd_x$v %*% solve(Sigma^2 + lambda_I) %*% Sigma %*% t(svd_x$u) %*% Y
  ret <- list(coefficients = beta, form = f)
  class(ret) <- "my_ridge"
  ret
}
