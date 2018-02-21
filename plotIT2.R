source('trapmf.R')

myplotIT2 = function(xUMF){
  
  A=xUMF;
  domain=c(A[1], A[4])
  xUMF=seq(domain[1],domain[2],length=1000)
  if (A[4]< A[3] + 10 ^ -5){
    A[4] = A[3] + 10^ -5;
  }
  
  if (A[8]< A[7] + 10 ^ -5){
    A[8] = A[7] + 10^ -5;
  }
  xLMF=xUMF;
  
  uUMF=trapmf(xUMF,A[1:4])
  uLMF=trapmf(xLMF,A[5:8]) * A[9]
  
  if (A[1] == 0 && A[2] == 0){
    shape = 1
  }else if( A[3] == 10 && round(A[4]) == 10){
    shape = 3
  }else{
    shape = 2
  }
  
  plot.new()
  plot(xUMF,uUMF,type="l", xlim=c(0, 10))
  par(new=TRUE)
  lines(xLMF[length(xLMF):1],uLMF[length(uLMF):1], type="l" )

  if (shape == 1 ){# % left shoulder
    highlight = c(0, A[3])
  }else if (shape == 2){# % interior
    highlight = c(A[2], A[3])
  }else if( shape == 3){# % right shoulder
    highlight = c(A[2], 10)
  }
  lines(highlight, c(1, 1))
  if( ((abs(A[3] - A[4]) < 10^-3) && (shape == 1))){
    coor=c(A[3], A[3])
    lines(coor, c(0, 1))
  }
  
  if( ((abs(A[1] - A[2]) < 10^-3) && (shape == 3))){
    coor = c(A[1], A[1])
    lines(coor, c(0, 1))
  }
  
  if( ((abs(A[1] - A[2]) < 10^-3) && (shape == 2))){
    coor = c(A[1],A[1])
    lines(coor, c(0, 1))
  }
  if( ((abs(A[3] - A[4]) < 10^-3) && (shape == 2))){
    coor=c(A[3], A[3])
    lines(coor, c(0, 1))
  }
  
  
  
}
