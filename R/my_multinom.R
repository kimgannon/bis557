#' @title Implement multinomial logistic regression
#' @description This function implements multinomial logistic regression, generalizing to k classes
#' @param Y a factor vector, the response variable
#' @param X a model matrix, the X variables as columns
#' @param lr the learning rate
#' @param max_n max number of iterations (a positive integer)
#' @param tol the difference threshold for which we will exit the algorithm for iterations less than max_n
#' @export

my_multinom <- function(X, Y, lr, max_n, tol=1e-10) {
  #development parameters - delete when finished
  library(palmerpenguins)
  data("penguins")
  form <- species ~ .
  mmx <- bis557::make_model_matrices(form, penguins)
  X <- mmx$X
  Y <- mmx$Y
  lr <- 1e-9
  max_n <- 10000
  tol <- 1e-10

   factors_y <- unique(Y)

  #the mu function for all logistic regression:
  mu_function <- function(eta) {exp(eta)/(1 + exp(-eta))}

  foreach::`%do%`(
    foreach::foreach(i = seq_along(factors_y)), {
      i = 1
      #determine one-vs-all link function for a given factor i
      Y_equals_i <- as.numeric((Y == factors_y[i]))



      #run glm with momentum gradient descent:
      output <- bis557::glm_momentum(X, Y_equals_i,
                          mu_fun=mu_fun, max_n= max_n,
                           lr=lr, tol=tol)

    } )

}


