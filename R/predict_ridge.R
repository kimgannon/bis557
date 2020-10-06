#' @title Prediction function for class my_ridge
#' @description This function predicts a value of Y based on my_ridge
#' @param object the my_ridge object
#' @param ... the data frame used to fit the object
#' @export
#' @examples
#' data("iris")
#' irisdf <- as.data.frame(iris)
#' irissamdf <- irisdf[sample(nrow(irisdf), 0.2*nrow(irisdf)), ]
#' irisform <- Sepal.Length ~.
#' obj <- my_ridge(irisform, irissamdf, 0.02)
#' predict_ridge(obj, irisdf)


predict_ridge <- function (object, ...) {
  dots <- list(...)
  d <- dots[[1]]
  if (!inherits(d, "data.frame")) {
    stop("Second argument must be a data frame.")
  }
  m <- stats::model.matrix(object$f, d)
  m %*% object$coefficients
}
