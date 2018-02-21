## extraction Level 2 membership function (S-shape) for fsQCA
# Author: Mehdi Korjani: korjani@gmail.com
# 2-20-2018

## use HMA algorithm and centoridIT2 
source('HMA.R')
source('centroidIT2.R')
source('plotIT2.R')


# define end points for one subject interval
## 1- On the scale of l to r, what are the endpoints of an interval of numbers ([aL,bL]) that you associate with the
# left end-point of the word
## 2- On the scale of l to r, what are the endpoints of an interval of numbers ([aR,bR]) that you associate with the
# right end-point of the word
# dataL = c(aL1, aR1, aL2, aR2, aL3, aR3)
# dataR = c(bL1, bR1, bL2, bR2, bL3, bR3)
dataL = c(320,	440,	467.5,	550,    720,	990)
dataR = c(380,  480,	550,  	632.5,	810,	1200)
Range = c(300,1200)

## normalized the interval
NormL=(dataL-Range[1])/(Range[2]-Range[1])*10
NormR=(dataR-Range[1])/(Range[2]-Range[1])*10

## generate 50 intervals based on the user interval
## find FOU using HMA algorithm
## Calculate centroid for FOUs
result = list()
FOU=list()
for(i in 1:3){
  # grab left and right intervals
  LeftInterval=c(NormL[2*i-1], NormR[2*i-1])
  RightInterval=c(NormL[2*i],NormR[2*i])
  
  # find 50 normal random number in each inteval
  leftEndPoints  = rnorm(50, mean = mean(LeftInterval), sd = sd(LeftInterval) )
  rightEndPoints = rnorm(50, mean = mean(RightInterval), sd = sd(RightInterval) )
  
  ## cut number outside range(0,10)
  leftEndPoints = pmax(leftEndPoints, 0)
  rightEndPoints= pmin(rightEndPoints,10)

  result[[i]] = HMA(leftEndPoints, rightEndPoints)
  center=centroidIT2(result[[i]]$MF)
  FOU[[i]]=c(result[[i]]$MF,center$CA, center$CAl, center$CAr)
}

## 3 words Ragin words
MFsLow= FOU[[1]][1:9]
MFsMod= FOU[[2]][1:9]
MFsHig= FOU[[3]][1:9]
CenterLow = FOU[[1]][10]
CenterModerate = FOU[[2]][10]
CenterHigh = FOU[[3]][10]

plot.new()
myplotIT2(MFsLow)
par(new=TRUE)
myplotIT2(MFsMod)
par(new=TRUE)
myplotIT2(MFsHig)



