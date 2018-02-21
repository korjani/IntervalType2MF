trapmf = function(x, params){
  #TRAPMF Trapezoidal membership function.
  #   TRAPMF(X, PARAMS) returns a matrix which is the trapezoidal
  #   membership function evaluated at X. PARAMS = [A B C D] is a 4-element
  #   vector that determines the break points of this membership function.
  #   We require that A <= B and C <= D. If B >= C, this membership
  #   function becomes a triangular membership function that could have
  #   a height less than unity. (See the example below.)
  #
  #   For example:
  #
  #       x = (0:0.1:10)';
  #       y1 = trapmf(x, [2 3 7 9]);
  #       y2 = trapmf(x, [3 4 6 8]);
  #       y3 = trapmf(x, [4 5 5 7]);
  #       y4 = trapmf(x, [5 6 4 6]);
  #       plot(x, [y1 y2 y3 y4]);
  #       set(gcf, 'name', 'trapmf', 'numbertitle', 'off');
  #
  #   See also DSIGMF, EVALMF, GAUSS2MF, GAUSSMF, GBELLMF, MF2MF, PIMF, PSIGMF,
  #   SIGMF, SMF, TRIMF, ZMF.

  
  if (length(params) < 4) {
    return('The trapezoidal MF needs at least four parameters.')
  }
  
  a = params[1]
  b = params[2]
  c = params[3]
  d = params[4]
  
  if (a > b) {
    return('Illegal parameter condition: a > b')
  }else if ( c > d){
    return('Illegal parameter condition: c > d')
  }
  
  y1 = matrix(0, 1,length(x))
  y2 = matrix(0, 1, length(x))
  
  # Compute y1
  index = which(x >= b)
  if (length(index) !=0){
    y1[index] = matrix(1,length(index))
  }
  index = which(x < a)
  if( length(index) !=0){
    y1[index] = matrix(0,length(index))
  }
  index = which(a <= x & x < b)
  if (length(index)!=0 & a != b){
    y1[index] = (x[index]-a)/(b-a)
  }
  
  
  # Compute y2
  index = which(x <= c)
  if (length(index)!=0){
    y2[index] = matrix(1, length(index))
  }
  index = which(x > d)
  if (length(index)!=0){
   y2[index] = matrix(0,length(index))
  }
  index = which(c < x & x <= d)
  if (length(index) !=0 & c != d){
    y2[index] = (d-x[index])/(d-c)
  }
  
  # Compute y
  y = pmin(y1, y2)
  return(y)
}