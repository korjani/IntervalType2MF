source('ToleranceFactor.R')
library(pracma)

HMA = function(L, R){
  
  # Bad data processing, see Equation (1) in paper
  for(i in length(L):1){
    if ( L[i] < 0 | L[i] > 10 | R[i] <0  | R[i] > 10 |  R[i] <= L[i] | R[i]-L[i] >= 10){ 
      L = L[ -i]
      R = R[ -i]
    }
  }
  nums=length(L)
  
  # Outlier processing
  intLeng = R-L;
  left = sort(L)
  right = sort(R)
  leng = sort(intLeng)
  
  NN1 = floor(nums * 0.25)
  NN2 = floor(nums * 0.75)
  
  # Compute Q(0.25), Q(0.75) and IQR for left-ends
  QL25 = left[NN1]*(1-(0.25*nums)%%1) + left[NN1+1]*((0.25*nums)%%1)
  QL75 = left[NN2]*(1-(0.75*nums)%%1) + left[NN2+1]*((0.75*nums)%%1)
  LIQR = QL75 - QL25
  
  # Compute Q(0.25), Q(0.75) and IQR for right-ends.
  QR25 = right[NN1]*(1-(0.25*nums)%%1) + right[NN1+1]*((0.25*nums)%%1)
  QR75 = right[NN2]*(1-(0.75*nums)%%1) + right[NN2+1]*((0.75*nums)%%1)
  RIQR = QR75 - QR25;
  
  ## outlier processing for L and R
  for(i in nums:1){
    if( L[i] < QL25-1.5*LIQR | L[i] > QL75+1.5*LIQR | R[i] < QR25-1.5*RIQR | R[i] > QR75+1.5*RIQR){
      L = L[ -i]
      R = R[ -i]
      intLeng = intLeng[ -i]
    }
  }
  n1 = length(L)
  
  ## Compute Q(0.25), Q(0.75) and IQR for interval length.
  NN1 = floor(n1 * 0.25)
  NN2 = floor(n1 * 0.75)
  QLeng25 = leng[NN1]*(1-(0.25*n1)%%1) + leng[NN1+1]*((0.25*n1)%%1)
  QLeng75 = leng[NN2]*(1-(0.75*n1)%%1) + leng[NN2+1]*((0.75*n1)%%1)
  lengIQR = QLeng75 - QLeng25
  
  ##  outlier processing for interval length
  for (i in n1:1){
    if (intLeng[i] < QLeng25-1.5*lengIQR | intLeng[i] > QLeng75+1.5*lengIQR){
      L = L[ -i]
      R = R[ -i]
      intLeng = intLeng[ -i]
    }
  }
  n1 = length(L)
  nums=c(nums, n1); # m'
  
  ## Tolerance limit processing, see Equation (3) in paper
  meanL = mean(L)
  stdL = sd(L)
  meanR = mean(R)
  stdR = sd(R)
  
  K=c(32.019, 32.019, 8.380, 5.369, 4.275, 3.712, 3.369, 3.136, 2.967, 2.839,
     2.737, 2.655, 2.587, 2.529, 2.48, 2.437, 2.4, 2.366, 2.337, 2.31,
     2.31, 2.31, 2.31, 2.31, 2.208)
  k=K[min(n1,25)]
  
  ## Tolerance limit processing for L and R
  for (i in n1:1){
    if (L[i] < meanL-k*stdL | L[i]>meanL+k*stdL | R[i] < meanR-k*stdR | R[i] > meanR+k*stdR){
      L = L[ -i]
      R = R[ -i]
      intLeng = intLeng[ -i]
    }
  }
  n1 = length(L)
  
  ## Tolerance limit processing for interval length
  meanLeng = mean(intLeng)
  stdLeng = sd(intLeng)
  
  k=min(c(K[min(n1,25)], meanLeng/stdLeng,(10-meanLeng)/stdLeng))

  for (i in n1:1){
    if (intLeng[i] < meanLeng-k*stdLeng | intLeng[i] > meanLeng+k*stdLeng){
      L = L[ -i]
      R = R[ -i]
      intLeng = intLeng[ -i]
    }
  }
  n1 = length(L)
  nums=c(nums, n1) #m''
  
  ## Reasonable interval processing, see Equation (4)-(6) in paper
  meanL = mean(L)
  stdL = sd(L)
  meanR = mean(R) 
  stdR = sd(R)
  
  # Determine sigma*
  if (stdL == stdR){
    barrier = (meanL + meanR)/2
  }else if ( stdL == 0){
    barrier = meanL+0.01
  }else if (stdR == 0){
    barrier = meanR-0.01
  }else {
    barrier1 =(meanR*stdL^2-meanL*stdR^2 + stdL*stdR*sqrt((meanL-meanR)^2+2*(stdL^2-stdR^2)*log(stdL/stdR)))/(stdL^2-stdR^2)
    barrier2 =(meanR*stdL^2-meanL*stdR^2 - stdL*stdR*sqrt((meanL-meanR)^2+2*(stdL^2-stdR^2)*log(stdL/stdR)))/(stdL^2-stdR^2)
    if (barrier1 >= meanL & barrier1 <= meanR) {
      barrier = barrier1
    } else {
      barrier = barrier2
    }
  }
  
  ## Reasonable interval processing
  for (i in n1:1){
    if (L[i] >= barrier | R[i] <= barrier | L[i] < 2*meanL-barrier | R[i] > 2*meanR-barrier) {
      L = L[ -i]
      R = R[ -i]
      intLeng = intLeng[ -i]
    }
  }
  n=length(L);
  nums=c(nums,  n) # m
  
  meanL = mean(L)
  stdL = sd(L)
  meanR = mean(R)
  stdR = sd(R)
  
  k = toleranceFactor(n1, 'TwoSided')
  
  ## determine what is the shape of membership function: left right, interior
  if (meanL-k*stdL <= 0){
    shape = 1  # left
  }else if (meanR + k*stdR >= 10){
    shape = 3  # right
  }else{
      shape = 2   # interior
  }
  
  ## calculate FOU foot print of uncertaity 
  if (shape == 1) {
    ##  left shoulder
    ## find the overlap by computing the CI for L and R
    switchPoint = min(R)
    
    nums=c(nums, length(R))
    
    FSL = matrix(0, 1, length(L))
    FSR = matrix(0, 1, length(R))
    if (sum(R - switchPoint) == 0) {
      UMF = c(0, 0, switchPoint, switchPoint)
      LMF = c(0, 0, switchPoint, switchPoint, 1)
      
      shape=1;
      MF=c(UMF, LMF)
      return(list(MF=MF, nums=nums, shape=shape, FSL=FSL, FSR=FSR))
    }
    
    ## side parts
    ## approach 4
    ## Consider the FOU as IT2 FS directly
    ## Map the mean of the mean of the data intervals to the center of
    ## centroid of the FOU; and also map the mean of the std of the data
    ## intervals to the center of centroid of the FOU
    c = switchPoint
    subsetRightLength = R - c
    
    ## remove the empty intervals
    ind = subsetRightLength <= 0
    subsetRightLength = subsetRightLength[-ind]
    ## calculate the mean and sd of the lengths
    lengthMean = mean(subsetRightLength)
    lengthSD = sd(subsetRightLength)
    
    ## map the center of the std and upper mean
    d = min(c + 3 * sqrt(2) * lengthSD, 10)
    i = min(6 * (c + lengthMean) - 4 * c - d, 10)
    
    bl = max(min(d, i), c)
    br = max(d, i)
    
    UMF = c(0, 0, switchPoint, br)
    LMF = c(0, 0, switchPoint, bl, 1)
    
  }else if (shape == 3){
    ## right shoulder
    ## find the overlap by computing the CI for L and R
    switchPoint = max(L)
    nums=c(nums, length(L)) #m*
      
    FSL = matrix(0, 1, length(L))
    FSR = matrix(0, 1, length(R))
    
    if (sum(L - switchPoint) == 0){
      UMF = c(switchPoint, switchPoint, 10, 10)
      LMF = c(switchPoint, switchPoint, 10, 10, 1)
      shape = 3
      MF=c(UMF, LMF)
      return(list(MF=MF, nums=nums, shape=shape, FSL=FSL, FSR=FSR))
    }
    ## side parts
    ## approach 4
    ## Consider the FOU as IT2 FS directly
    ## Map the mean of the mean of the data intervals to the center of
    ## centroid of the FOU; and also map the mean of the std of the data
    ## intervals to the center of std of the FOU
    c = switchPoint
    subsetRightLength = c - L;
    
    ## remove the empty intervals
    ind = subsetRightLength <= 0
    subsetRightLength = subsetRightLength[-ind]

    ## calculate the mean and sd of the lengths
    lengthMean = mean(subsetRightLength)
    lengthSD = sd(subsetRightLength)
    
    
    ## map the center of the std and lower mean
    a = max(0, c - 3 * sqrt(2) * lengthSD)
    e = max(6 * (c - lengthMean) - 4 * c - a, 0)
    
    
    al = min(a, e)
    ar = max(e, a)
    
    UMF = c(al, switchPoint, 10, 10)
    LMF = c(ar, switchPoint, 10, 10, 1)
    
  }else{
    ## find the overlap by computing the one-sided CI for L and R
    overlapLeft = max(L)
    overlapRight = min(R)
    
    nums=c(nums, length(L), length(R)) #m*'s
    
    FSL = matrix(0, length(L), 1)
    FSR = matrix(0, length(R), 1)
    
    ## side parts
    # approach 4
    # Consider the FOU as IT2 FS directly
    # Map the mean of the mean of the data intervals to the center of
    # centroid of the FOU; and also map the mean of the std of the data
    # intervals to the center of centroid of the FOU
    c = overlapLeft;
    subsetRightLength = c - L;
    
    ## remove the empty intervals
    ind = subsetRightLength <= 0
    subsetRightLength = subsetRightLength[-ind]
    
    ## calculate the mean and sd of the lengths
    lengthMean = mean(subsetRightLength)
    lengthSD = sd(subsetRightLength)
    
    
    ## map the center of the std and lower mean
    a = max(0, c - 3 * sqrt(2) * lengthSD)
    e = max(0, 6 * (c - lengthMean) - 4 * c - a)
    
    al = min(a, e)
    ar = min(max(e, a), c)
  
    c = overlapRight
    subsetRightLength = R - c
    
    ## remove the empty intervals
    ind = subsetRightLength <= 0
    subsetRightLength = subsetRightLength[-ind]
    ## calculate the mean and sd of the lengths
    lengthMean = mean(subsetRightLength)
    lengthSD = sd(subsetRightLength)
    
    ## map the center of the std and upper mean
    d = min(c + 3 * sqrt(2) * lengthSD, 10)
    i = min(6 * (c + lengthMean) - 4 * c - d, 10)
    
    bl = max(min(d, i), c)
    br = max(d, i)
    
    UMF = c(al, overlapLeft, overlapRight, br)
    LMF = c(ar, overlapLeft, overlapRight, bl, 1)
    
  }
  MF=c(UMF, LMF)
  return(list(MF=MF, nums=nums, shape=shape, FSL=FSL, FSR=FSR))
  
  
}