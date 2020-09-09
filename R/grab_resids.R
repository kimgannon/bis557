
#' @title Grab the residuals
#' @description This function grabs the residuals of a linear model.
#' @param lm_obj the linear model created by the lm function
#' @export
#' @examples
#' library(palmerpenguins)
#' fit <- lm (bill_length_mm ~., data = penguins[,-8])
#' grab_resids(fit)

grab_resids <- function(lm_obj) {
  lm_obj$residuals
}
