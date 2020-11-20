import numpy as np

def python_ridge(Y_name, X_names, data, l):
  '''This function computes ridge regression estimators in Python.
  Args: 
    Y_name - a list containing the name of the Y variable (a list of strings, of length 1)
    X_names - a list containing the name of the X variables (a list of strings, of length at least 1)
    data - Python-readable data
    l - lambda, the penalty parameter
    
  Returns:
    List of names and corresponding coefficients (includes constant as noted)
    '''
  X_noconstant = data[X_names].values
  #add a constant
  len_X = len(data[X_names[0]].values)
  constant = np.ones((len_X, 1))
  X = np.append(constant, X_noconstant, axis=1)
  Y = data[Y_name].values

  u, s, vh = np.linalg.svd(X, full_matrices = False)

  Sigma = np.diag(s)
  s_length = len(s)
  lambda_array = np.array([l])
  lambda_I_input = np.repeat(l, [s_length], axis = 0)
  lambda_I = np.diag(lambda_I_input)
  inv = np.linalg.inv(Sigma @ Sigma + lambda_I)

  beta = np.matrix.transpose(vh) @ inv @ Sigma @ np.matrix.transpose(u) @ Y

  #return object
  beta_list = np.ndarray.tolist(beta)
  X_names = ["constant"] + X_names
  ret = [X_names, beta_list]
  return ret
