% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_model.R
\name{linear_model}
\alias{linear_model}
\title{Create linear model}
\usage{
linear_model(f, d, contrasts = NULL)
}
\arguments{
\item{f}{the model to which data will be fit (a formula)}

\item{d}{the data frame containing data to be fit (a data frame)}

\item{contrasts}{a list of variable names to be used as factor variables}
}
\description{
This function creates a linear model of a formula from a data frame and
a list of factor variables.
}
\examples{
library(palmerpenguins)
data("penguins")
penguinsform <- bill_length_mm ~ body_mass_g
penguinsdf <- (as.data.frame(penguins))
penguinscont <- list(Species = "contr.sum")
linear_model(penguinsform, d=penguinsdf, contrasts=penguinscont)
}
