#' @title Ridge regression for collinear/near-collinear columns
#' @description This function fits a ridge regression model, accounting for collinear/near collinear columns.
#' @param f a formula the data will be fit too
#' @param lambda the adjustment/"penalty" parameter
#' @export
#' @examples
#' data("iris")
#' irisform <- Sepal.Length ~.
#' lm_graddescent_xval(irisform, iris, .01)

