source('EKM.R')
source('mg.R')

centroidIT2 = function(A){
  
  #
  # [CA, CAl, CAr]=centroidIT2(A)
  #
  # To compute the centroid of an IT2 FS, which is defined by nine
  # parameters (p1, p2, p3, p4, p5, p6, p7, p8, p9) shown in Figure of README.md. 
  # 
  # Author: Mehdi Korjani: korjani@gmail.com
  #
  # A: an IT2 FS represented by 9 parameters.
  # CA: center of centroid of A
  # CAl: left bound of the centroid
  # CAr: right bound of the centroid
  
  if (length(A)!=9){
    return('The input vector must be a 9-point representation of an IT2 FS.')
  }
  
  Xs=seq(A[1],A[4],length=100)
  UMF=mg(Xs, A[1:4],c(0, 1, 1, 0) )
  LMF=mg(Xs, A[5:8],c(0, A[9], A[9], 0) )
  CAl=EKM(Xs, LMF, UMF, -1)
  CAr=EKM(Xs, LMF, UMF, 1)
  CA=(CAl+CAr)/2
  
  return(list(CA=CA, CAl=CAl, CAr=CAr))
}
