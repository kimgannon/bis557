
#' @title Grab the slope coefficients
#' @description This function grabs the coefficients of a linear model.
#' @param lm_obj the linear model created by the lm function
#' @export
#' @examples
#' library(palmerpenguins)
#' fit <- lm (bill_length_mm ~., data = penguins[,-8])
#' grab_coefs(fit)

grab_coefs <- function(lm_obj) {
    lm_obj$coefficients
}
