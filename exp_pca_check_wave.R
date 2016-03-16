source("./utils.R")
source("./readData.R")
source("./getMatrixByMag.R")
source("./addPCAData.R")

plot(globalAccs[200:500,2],type = "l")

plot(top$ConvertedData2[200:500],type = "l")

addDeviceDataToPCA(top,"top")
addDeviceDataToPCA(leftHand,"leftHand")
addDeviceDataToPCA(rightHand,"rightHand")
addDeviceDataToPCA(rightPants,"rightPants")
addDeviceDataToPCA(leftPants,"leftPants")

epca6A <- prcomp(pcaData,
                 center = TRUE,
                 scale. = FALSE)
newData = predict(epca6A, newdata = pcaData)
pcaForward  =-newData[200:500,1]
plot(pcaForward,type = "l", main = "multi device pca on horizontal",ylab = "acc(m/s^2)", cex.lab=1.5,cex.main=1.5)

plotForwardAndCorWithPCA = function(mdata,tag){
  cvdata = mdata[200:500,]$ConvertedData1
  gacc2 = getGlobalAccByMag(mdata[200:500,])[,1]
  corv = round(cor(pcaForward,cvdata),3)
  corv2 = round(cor(pcaForward,gacc2),3)
  plot(cvdata,type = "l", main = paste(tag,"forward correlation:",corv,corv2),ylab = "acc(m/s^2)", cex.lab=1.5,cex.main=1.5)
}

plotForwardAndCorWithPCA(top, "top")
plotForwardAndCorWithPCA(leftHand, "leftHand")
plotForwardAndCorWithPCA(rightHand, "rightHand")
plotForwardAndCorWithPCA(leftPants, "leftPants")
plotForwardAndCorWithPCA(rightPants, "rightPants")



