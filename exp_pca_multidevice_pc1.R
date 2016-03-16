source("./utils.R")
source("./readData.R")
source("./getMatrixByMag.R")
source("./addPCAData.R")

mydata = top
getSingleDevicePC1 = function(mydata){
  laccs  = cbind(mydata$LinearAcc0, mydata$LinearAcc1, mydata$LinearAcc2)
  epca6A <- prcomp(laccs,
                   center = TRUE,
                   scale. = FALSE)
  newData = predict(epca6A, newdata = laccs)
  return(newData[,1])
}

addDeviceDataPC1ToPCAData = function(mydata){
  singleDevicePC1Data = getSingleDevicePC1(mydata)
  addPCAData(singleDevicePC1Data)
}

addDeviceDataPC1ToPCAData(top)
addDeviceDataPC1ToPCAData(leftHand)
addDeviceDataPC1ToPCAData(rightHand)
addDeviceDataPC1ToPCAData(leftPants)
addDeviceDataPC1ToPCAData(rightPants)

epca6A <- prcomp(pcaData,
                 center = TRUE,
                 scale. = FALSE)
newData = predict(epca6A, newdata = pcaData)
pcaForward  =newData[200:500,1]
plot(pcaForward,type = "l", main = "multi device pca on horizontal",ylab = "acc(m/s^2)", cex.lab=1.5,cex.main=1.5)

plotForwardAndCorWithPCA = function(mdata,tag){
  cvdata = mdata[200:500,]$ConvertedData1
  gacc2 = getGlobalAccByMag(mdata[200:500,])[,2]
  corv = round(cor(pcaForward,cvdata),3)
  # corv2 = round(cor(pcaForward,gacc2),3)
  plot(cvdata,type = "l", main = paste(tag,"forward correlation:",corv),ylab = "acc(m/s^2)", cex.lab=1.5,cex.main=1.5)
}

plotForwardAndCorWithPCA(top, "top")
plotForwardAndCorWithPCA(leftHand, "leftHand")
plotForwardAndCorWithPCA(rightHand, "rightHand")
plotForwardAndCorWithPCA(leftPants, "leftPants")
plotForwardAndCorWithPCA(rightPants, "rightPants")
