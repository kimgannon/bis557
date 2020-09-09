context("Testing grab coefficients")

library(palmerpenguins)
data("penguins")
fit <- lm (bill_length_mm ~., data = penguins[,-8])
resids <- as.vector(grab_resids(fit))

expect_equal(round(resids), round(as.vector(fit$residuals)))
