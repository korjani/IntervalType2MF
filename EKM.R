EKM = function(xPoint,wLower,wUpper,maxFlag){
  
  
  # y = EKM(xPoint,wLower,wUpper,maxFlag)
  #
  # function to implement the EKM algorithms
  #
  # Mehdi Korjani (korjani@gmail.com)
  # xPoint: x_i
  # [wLower, wUpper]: range of w_i
  # maxFlag: 1, if to output the maximum; -1, if to output the minimum
  # xPoint, wLower and wUpper must have the same length.
  
  if ( max(wUpper)==0 | max(xPoint)==0) {
    y=0
    return(y)
  }
  
  if(max(wLower)==0) {
    if(maxFlag>0) {
      y=max(xPoint)
    }else{
      y=min(xPoint)
    }
    return(y)
  }

  
  if(length(xPoint)==1){
    y=xPoint
    return(y)
  }
  
  # combine zero firing intervals
  I=which(wUpper==0)
  xPoint = xPoint[-I]
  wLower = wLower[-I]
  wUpper = wUpper[-I]
  
  # combine zero xs
  m = sort(xPoint, index.return=TRUE)
  xIndex = m$ix
  xSort = m$x
  
  lowerSort = wLower[xIndex]
  upperSort = wUpper[xIndex]
  k=which(rev(xSort)==0)
  #k = k[1]
  
  if(length(k) !=0 && (length(xSort) -k[1] + 1)>1){
    xSort[1]=0
    ind = 2:k
    xSort = xSort[-ind]
    lowerSort[1]=sum(lowerSort[1:k])
    lowerSort=lowerSort[-ind]
    upperSort[1]=sum(upperSort[1:k])
    upperSort = upperSort[-ind]
  }
  
  
  ly=length(xSort)
  if(maxFlag<0){
    k=round(ly/2.4)
    temp=c(upperSort[1:k], lowerSort[(k+1):ly])
  }else{
    k=round(ly/1.7)
    temp=c(lowerSort[1:k], upperSort[(k+1):ly])
  }
  a=sum(temp*xSort)
  b=sum(temp)
  y = a/b
  kNew = which(xSort > y)
  kNew = kNew[1] - 1
  
  while(k != kNew){
    mink=min(k,kNew)
    maxk=max(k,kNew)
    temp=upperSort[(mink+1):maxk]-lowerSort[(mink+1):maxk]
    b=b-sign(kNew-k)*sign(maxFlag)*sum(temp)
    a=a-sign(kNew-k)*sign(maxFlag)*sum(temp*xSort[(mink+1):maxk])
    y = a/b
    k=kNew
    kNew = which(xSort>y)
    kNew = kNew[1] - 1
  }
  return(y)
}
