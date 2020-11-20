def lenet(Y, X, l, alph, bta, W, tol=0.00001):
  '''This function implements lenet (linear elastic net) with coordinate descent.
  Args: 
   Y - a vector containing Y values
    X - a matrix containing X values
    l - lambda, the penalty parameter
    alph - alpha, the relative weight of the l1 vs l2 penalty (in the case of LASSO, alpha = 1)
    bta - an initial guess of beta
    W - a weight
    tol - a tolerance level

  Returns: Regression vector beta of length ncol(X)
  '''
  #Update slope coefs until they converge
  import numpy as np
  for j in range(1000):
    bta_old = bta.copy()
    bta = lenet_update(Y, X, l, alph, bta, W)
    abs_diff = np.absolute(np.subtract(bta, bta_old))
    len_ad = len(abs_diff)
    abs_diff = abs_diff.reshape(len(abs_diff),1)
    tol_vec = np.ones((len(abs_diff),1))*tol
#breakout condition
    exit = True
    for k in range(len_ad):
      if abs_diff[k,0] >= tol_vec[k,0]:
        exit = False
    if exit == True:
      print("Total number of iterations: ", j)
      return bta
    elif j == 99:
      print("Function did not converge after 100 iterations")
      return bta
