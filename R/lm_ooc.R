#' @title Out-of-core LM implementation
#' @description This function uses the Python script lm_calc to fit a linear model using small chunks of observations, rather than the whole data set at once.
#' @param form a formula
#' @param d_name the name of the data to be fit (a CSV file)
#' @param nrows the chunk size
#' @export
#' @examples
#' data("iris")
#' library(reticulate)
#' reticulate::source_python('../python/Scripts/lm_calc.py')
#' reticulate::use_condaenv("r-reticulate")
#' data(iris)
#' write.table(x = iris, file = "iris.csv", sep = ",", row.names = FALSE)
#' irisform <- Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
#' beta_hats <- bis557::lm_ooc(irisform, "iris.csv", nrows=75)
#' print(beta_hats)

lm_ooc <- function(form, d_name,  nrows) {
  it <- iterators::iread.table(d_name, row.names=NULL, header=TRUE, nrows=nrows, sep = ",")
  beta_vec <- foreach::`%do%`(
    foreach::foreach(d_sub=it, .combine = cbind
    ),
    {
    mmx <- bis557::make_model_matrices(form,d_sub)
    X <- mmx$X
    Y <- mmx$Y
    lm_calc(X,Y)
      }
    )
  betas <- rowMeans(beta_vec)
  betas
}
