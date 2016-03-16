source("./utils.R")
source("./timealign_3_2.R")
source("./getMatrixByMag.R")
source("./addPCAData.R")

getCorMatrix = function(ds){
  corMatrix = matrix(nrow = 5,ncol = 5)
  for(i in 1:5){
    for(j in 1:5){
      corMatrix[i,j] = cor(ds[,i],ds[,j])
    }
  }
  rownames(corMatrix) = c("top","lefthand","righthand","leftpants","rightpants")
  colnames(corMatrix) = c("top","lefthand","righthand","leftpants","rightpants")
  return(corMatrix)
}

getfreq = function(mdata){
  mdata = fft(mdata)
  mdata = mdata[2:(length(mdata)/2)]
  mdata = as.numeric(mdata)
  return(mdata)
}
getMagnitude = function(mdata){
  return (sqrt(mdata$LinearAcc0^2+mdata$LinearAcc1^2+mdata$LinearAcc2^2))
}
ds1 = cbind(getMagnitude(top),getMagnitude(leftHand),getMagnitude(rightHand),getMagnitude(leftPants),getMagnitude(rightPants))
corMatrixTime = getCorMatrix(ds1)

ds2 = cbind(getfreq(getMagnitude(top)),getfreq(getMagnitude(leftHand)),getfreq(getMagnitude(rightHand)),getfreq(getMagnitude(leftPants)),getfreq(getMagnitude(rightPants)))
corMatrixFreq = getCorMatrix(ds2)

corMatrixTime

corMatrixFreq

