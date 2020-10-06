#' @title Linear model using gradient descent, adjusted for out-of-sample accuracy
#' @description This function creates a linear model using gradient descent, adjusting the penalty for out-of-sample accuracy.
#' @param f a formula the data will be fit too
#' @param d the data to be fit (a data frame)
#' @param gamma the step size (a positive number)
#' @param n number of iterations (a positive integer)
#' @param flds a list of folds
#' @export
#' @examples
#' data("iris")
#' irisform <- Sepal.Length ~.
#' irisfold <- rsample::vfold_cv(iris)
#' lm_graddescent_xval(irisform, iris, 0.0001, 100000, irisfold)

lm_graddescent_xval <- function(f, d, gamma, n, flds) {
  #calculates the loss gd based on out-of-sample validity
  Y <- as.character(f)[2]
  os_resids_gd <- foreach::`%do%`(
    foreach::foreach(fold = flds$splits, .combine = c)
  , {
    #these two lines below will give us the loss from each fold
    fit <- bis557::lm_graddescent(f, rsample::analysis(fold),gamma,n)
    as.vector(matrix(rsample::assessment(fold)[,Y],ncol=1)) -
                as.vector(
                  bis557::predict_lm_graddescent(fit, rsample::assessment(fold)))
  } )

  }
