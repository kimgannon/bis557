def lenet_update(Y, X, l, alph, bta, W):
  '''This function updates a beta vector using coordinate descent in Python.
  Args: 
   Y - a vector containing Y values
    X - a matrix containing X values
    l - lambda, the penalty parameter
    alph - alpha, the relative weight of the l1 vs l2 penalty (in the case of LASSO, alpha = 1)
    bta - an initial guess of beta
    W - a weight value (everything will be weighted equally per CASL)

  Returns:
    Regression vector beta of length ncol(X)
    '''
  import numpy as np
  WX = W*X
  X2 = X*X
  WX2 = W*X2
  Xb = X @ bta
  for i in range(len(bta)):
    ith_of_X = X[:,i]
    ith_X_array = ith_of_X.reshape((len(Xb),1))
    Xb = Xb - ith_X_array*float(bta[i])
    WX_at_i = WX[:,i]
    Y_m_Xb = np.subtract(Y, Xb)
    summand = WX_at_i*Y_m_Xb
    sm = sum(summand)[0]
    threshold_val = l*alph
    bta[i] = soft_threshold(sm, threshold_val)
    sum_ith_WX2 = sum(WX2[:,i])
    bta[i] = bta[i]/(sum_ith_WX2 + l*(1-alph))
    Xb = Xb + ith_X_array*bta[i]
  return bta
