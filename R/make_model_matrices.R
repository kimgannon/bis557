#' @title Create model matrices
#' @description This function creates a model matrix using gradient descent. Written in class 09/30/20. NOTE - it removes all NAs
#' @param form a formula
#' @param d the data to be fit (a data frame)
#' @export
#' @examples
#' data("iris")
#' irisform <- Sepal.Length ~.
#' make_model_matrices(irisform, iris)

make_model_matrices <- function(form, d) {
  d_no_na <- stats::model.frame(form,d) #note: this doesn't actually remove NAs, as the name would suggest
  X <- stats::model.matrix(form, d_no_na)
  y_name <- as.character(form)[2]
  Y <- matrix(d_no_na[,y_name], ncol = 1)
  list(X=X, Y=Y)
}
