## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
  library(bis557)
  library(tibble)
  library(foreach)
  library(rsample)
  library(stats)

  irisform <- Sepal.Length~.
  folds <- rsample::vfold_cv(iris)
  os_resid_gd <- lm_graddescent_xval(irisform, iris, 0.0001, 100000, flds=folds)


  #lm resids
  Y <- as.character(irisform)[2]
  os_resids_lm <- foreach::`%do%`(
    foreach::foreach(fold = folds$splits, .combine = c)
  , {
    #these two lines below will give us the loss from each fold
    fit <- stats::lm(irisform, rsample::analysis(fold))
    as.vector(matrix(rsample::assessment(fold)[,Y],ncol=1)) -
                as.vector(
                  stats::predict(fit, rsample::assessment(fold)))
    
  } )
  difference <- os_resid_gd - os_resids_lm
  df <- cbind.data.frame(os_resid_gd, os_resids_lm, difference)
  tibble::as.tibble(df)
  

## -----------------------------------------------------------------------------
  library(bis557)
  library(tibble)
  library(foreach)
  library(rsample)
  library(stats)

  data("iris")
  irisform <- Sepal.Length~.
  folds <- rsample::vfold_cv(iris)
  
  #ridge resids for low lambda (lambda = 0.02)
  os_resids_ridge <- foreach::`%do%`(
    foreach::foreach(fold = folds$splits, .combine = c)
  , {
    fit <- bis557::my_ridge(irisform, rsample::analysis(fold), lambda = 0.02)
    as.vector(matrix(rsample::assessment(fold)[,Y],ncol=1)) -
                as.vector(
                  bis557::predict_ridge(fit, rsample::assessment(fold)))
    
  } )
  
  
  #lm resids
  Y <- as.character(irisform)[2]
  os_resids_lm <- foreach::`%do%`(
    foreach::foreach(fold = folds$splits, .combine = c)
  , {
    #these two lines below will give us the loss from each fold
    fit <- stats::lm(irisform, rsample::analysis(fold))
    as.vector(matrix(rsample::assessment(fold)[,Y],ncol=1)) -
                as.vector(
                  stats::predict(fit, rsample::assessment(fold)))
    
  } )
  
  #get difference, compare in tibble
  difference <- os_resids_ridge - os_resids_lm
  df <- cbind.data.frame(os_resid_gd, os_resids_lm, difference)
  tibble::as.tibble(df)



## -----------------------------------------------------------------------------
  library(bis557)
  library(tibble)
  library(foreach)
  library(rsample)
  library(stats)

data("iris")
dta <- iris
irisform <- Sepal.Length~.
folds <- rsample::vfold_cv(iris)
lambdas <- seq(0, 100, by = 0.1)
predicted <- my_ridge_xval(irisform,dta,lambdas,folds)

#Now I will plot several values of lambda and find the mean squared loss

Y <- as.character(irisform)[2]
    meansq_vec <-
      foreach::`%do%`(foreach::foreach(i = seq_along(lambdas), .combine = c),
                                  {foreach::`%do%`(
        foreach::foreach(fold = folds$splits, .combine = c)
        , {
          #these two lines below will give us the loss from each fold at lambda_i
          fit <- bis557::my_ridge(irisform, rsample::analysis(fold),lambdas[i])
          difference <- as.vector(matrix(rsample::assessment(fold)[,Y],ncol=1)) -
            as.vector(
              bis557::predict_ridge(fit, rsample::assessment(fold)))
    })
    diffsq <- difference^2
    mean(diffsq)
  })
    

candidates <- cbind.data.frame(lambdas,meansq_vec)
plot(candidates)

predicted


