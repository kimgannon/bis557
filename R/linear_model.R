
#' @title Create linear model
#' @description This function creates a linear model of a formula from a data frame and
#' a list of factor variables.
#' @param formula the model to which data will be fit
#' @param df the data frame containing data to be fit
#' @param contrasts_list a list of variable names to be used as factor variables
#' @export
#' @examples
#' library(palmerpenguins)
#' data("penguins")
#' penguinsform <- bill_length_mm ~ body_mass_g
#' penguinsdf <- (as.data.frame(penguins))
#' penguinscont <- list("species", "year")
#' linear_model(penguinsform, penguinsdf, penguinscont)


linear_model <- function(formula, df, contrasts_list) {
  stats::lm(formula, data=df, contrasts.arg=contrasts_list)
}
