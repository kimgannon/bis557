
#' @title Create linear model
#' @description This function creates a linear model of a formula from a data frame and
#' a list of factor variables.
#' @param f the model to which data will be fit (a formula)
#' @param d the data frame containing data to be fit (a data frame)
#' @param contrasts a list of variable names to be used as factor variables
#' @export
#' @examples
#' library(palmerpenguins)
#' data("penguins")
#' penguinsform <- bill_length_mm ~ body_mass_g
#' penguinsdf <- (as.data.frame(penguins))
#' penguinscont <- list(Species = "contr.sum")
#' linear_model(penguinsform, d=penguinsdf, contrasts=penguinscont)


linear_model <- function(f, d, contrasts=NULL) {
  #remove NAs in accordance with form
  d_na_rm <- stats::model.frame(f,d)
  X <- stats::model.matrix(f,d_na_rm, contrasts.arg=contrasts)
  Yn <- as.character(f)[2]
  Ymat <- matrix(d_na_rm[,Yn],ncol=1)

  #use SVD - adapted from Arnold, Kane, Lewis p. 18
  svd_X <- svd(X)
  r <- sum(svd_X$d >.Machine$double.eps)
  U <- svd_X$u[, 1:r]
  V <- svd_X$v[, 1:r]
  beta <- V %*% (t(U) %*% Ymat / svd_X$d[1:r])
  beta_names <- rownames(beta)
  beta <- as.numeric(beta)

  #handling collinearity in SVD output for computationally equivalent values
  for (i in 1:length(beta)) {
    if (abs(beta[i]) <= .000000001) {
      beta[i] = NA
    }
  }

  names(beta) <- beta_names
  ret <- list(coefficients = beta)
  ret
}
