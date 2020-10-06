#' @title Finding optimal lambda using out of sample cross validation
#' @description This function cross-validates a ridge regression model for out of sample accuracy for several lambdas, then finds the optimal lambda. We choose lambda that yields the lowest mean squared difference in out-of-sample residuals
#' @param f a formula the data will be fit to
#' @param d the data to be fit (a data frame)
#' @param lambdas a vector of candidate ridge parameters
#' @param flds a list of folds
#' @export
#' @examples
#' data("iris")
#' irisform <- Sepal.Length ~.
#' irisfold <- rsample::vfold_cv(iris)
#' lambda_vec <- seq(1,100,by=0.1)
#' my_ridge_xval(irisform, iris, lambda_vec, irisfold)


my_ridge_xval <- function(f, d, lambdas, flds) {
  #calculates the out-of-sample accuracy
  Y <- as.character(f)[2]
    meansq_vec <-
      foreach::`%do%`(foreach::foreach(i = seq_along(lambdas), .combine = c),
                                  {foreach::`%do%`(
        foreach::foreach(fold = flds$splits, .combine = c)
        , {
          #these two lines below will give us the loss from each fold at lambda_i
          fit <- bis557::my_ridge(f, rsample::analysis(fold),lambdas[i])
          difference <- as.vector(matrix(rsample::assessment(fold)[,Y],ncol=1)) -
            as.vector(
              bis557::predict_ridge(fit, rsample::assessment(fold)))
    })
    diffsq <- difference^2
    mean(diffsq)
  })

  #Now select lambda with the lowest mean squared residuals

  candidates <- cbind.data.frame(lambdas,meansq_vec)
  min_meansq <- min(candidates$meansq)
  #if there are multiple lambdas that map to the min (shouldn't be), pick the smallest
  ret <- min(candidates$lambdas[candidates$meansq == min_meansq])
  ret
}
