#' Update beta vector using coordinate descent.
#'
#' @param X A numeric data matrix.
#' @param y Response vector.
#' @param lambda The penalty term.
#' @param alpha Value from 0 and 1; balance between l1/l2 penalty.
#' @param b A vector of warm start coefficients for the algorithm.
#' @param W A vector of sample weights.
#'
#' @return A matrix of regression vectors with ncol(X) columns
#' and length(lambda_vals) rows.
#'
#' @author Taylor Arnold, Michael Kane, Bryan Lewis.
#'
#' @references
#'
#' Taylor Arnold, Michael Kane, and Bryan Lewis.
#' \emph{A Computational Approach to Statistical Learning}.
#' Chapman & Hall/CRC Texts in Statistical Science, 2019.
#'
#' @export
casl_lenet_update_beta <-
  function(X, y, lambda, alpha, b, W)
  {
    WX <- W * X
    WX2 <- W * X^2
    Xb <- X %*% b

    for (i in seq_along(b))
    {
      Xb <- Xb - X[, i] * b[i]
      b[i] <- casl_util_soft_thresh(sum(WX[,i, drop=FALSE] *
                                          (y - Xb)),
                                    lambda*alpha)
      b[i] <- b[i] / (sum(WX2[, i]) + lambda * (1 - alpha))
      Xb <- Xb + X[, i] * b[i]
    }
    b
  }
