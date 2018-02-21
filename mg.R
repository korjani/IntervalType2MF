mg = function(x,xMF,uMF){

  #
  # f=mg(x,xMF,uMF)
  #
  # function to compute the membership grades of x on a T1 FS
  #
  # Mehdi Korjani: korjani@gmail.com
  # xMF: x-coordinates of the T1 FS
  # uMF: u-coordinates of the T1 FS
  # u: membership of x on the T1 FS
  
  if (length(xMF) !=length(uMF)) {
    return('xMF and uMF must have the same length.')
  }
  
  m = sort(xMF, index.return=TRUE)
  index = m$ix
  xMF = m$x
  uMF=uMF[index]
  
  u=matrix(0, 1, length(x))
  for (i in 1:length(x)) {
    if (x[i] <= xMF[1] | x[i] >= xMF[length(xMF)]){
      u[i]=0
    }else{
      left=which(rev(xMF)<x[i])
      left= length(xMF) - left[1] + 1
      right=left+1
      u[i]=uMF[left]+(uMF[right]-uMF[left])*(x[i]-xMF[left])/(xMF[right]-xMF[left])
    }
  }
  return(u)
}
