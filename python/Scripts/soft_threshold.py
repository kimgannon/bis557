def soft_threshold(a,b):
  '''This function computes the soft threshold for the elastic net in Python.
  Args: 
   a - a number
   b - the threshold value (a float)

  Returns:
    Vector a with soft threshold values
    '''
  import numpy as np
  if a <= b:
    a = 0
  if a > 0:
    a = a - b
  if a < 0:
    a
  return a
