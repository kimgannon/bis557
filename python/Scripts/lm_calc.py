def lm_calc(X, Y):
  '''Calculates beta hat_lm for a given X Y
  
  Args:
    X - independent variable data
    Y - response variable data
  Returns:
    An estimate for beta hat
    '''
  import numpy as np
  Xt = np.matrix.transpose(X)
  XtX = Xt @ X
  XtXinv = np.linalg.inv(XtX)
  beta_hat = XtXinv @ Xt @ Y
  return beta_hat
