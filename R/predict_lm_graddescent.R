#' @title Prediction function for class lm_graddescent
#' @description This function predicts a value of Y based off a graddescent linear model
#' @param object the lm_graddescent object
#' @param ... the data frame used to fit the object
#' @export
#' @examples
#' data("iris")
#' irisdf <- as.data.frame(iris)
#' irissamdf <- irisdf[sample(nrow(irisdf), 0.2*nrow(irisdf)), ]
#' irisform <- Sepal.Length ~.
#' obj <- lm_graddescent(irisform, iris, 0.0001, 100000)
#' predict_lm_graddescent(obj, irisdf)


predict_lm_graddescent <- function (object, ...) {
  dots <- list(...)
  d <- dots[[1]]
  if (!inherits(d, "data.frame")) {
    stop("Second argument must be a data frame.")
  }
  m <- stats::model.matrix(object$f, d)
  m %*% object$coefficients
}
