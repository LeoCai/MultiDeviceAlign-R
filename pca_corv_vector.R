source("./utils.R")
source("./readData.R")
source("./getMatrixByMag.R")
source("./addPCAData.R")
library(zoo)

selectIndex = 200:500

computeFc = function(){
  addDeviceDataToPCA(top,"top")
  addDeviceDataToPCA(leftHand,"leftHand")
  addDeviceDataToPCA(rightHand,"rightHand")
  addDeviceDataToPCA(rightPants,"rightPants")
  addDeviceDataToPCA(leftPants,"leftPants")
  epca6A <- prcomp(pcaData,
                   center = TRUE,
                   scale. = FALSE)
  newData = predict(epca6A, newdata = pcaData)
  pcaForward=-newData[selectIndex,1]
  plot(pcaForward,type="l",main = "Fc")
  return(pcaForward)
}

computeFi = function(myData){
  firstData = myData[1,]
  gvInit = cbind(firstData$Gravity0,firstData$Gravity1,firstData$Gravity2)
  mgInit = cbind(firstData$MagnetData0,firstData$MagnetData1,firstData$MagnetData2)
  initRotationMatirx = getMatrixByMag(gvInit,mgInit)
  globalAccs = getGlobalAccByMag(myData)
  globalAccs[,3] = 0
  rotatedGlobal = c()
  for(i in 1:nrow(globalAccs)){
    rotatedAcc = rotationByMatrix(globalAccs[i,], t(initRotationMatirx))
    # rotatedAccOnHori = computeProjOnHoriSingleData(rotatedAcc,gvInit)
    rotatedGlobal = rbind(rotatedGlobal, rotatedAcc)
  }
  return(list(rotatedGlobal,initRotationMatirx))
}

computeAngleByPCA_COR = function(myData,tag){
  Fc = computeFc()
  plot(Fc, type="l", main = "Fc by PCA",cex.main = 1.5, cex.lab = 1.5)
  myData = myData[selectIndex,]
  firstData = myData[1,]
  FiResults = computeFi(myData)
  Fi = FiResults[[1]]
  
  
  #test
  computeFnData = c()
  for(i in 1:nrow(myData)){
    gv = cbind(myData[i,]$Gravity0,myData[i,]$Gravity1,myData[i,]$Gravity2)
    mag = cbind(myData[i,]$MagnetData0,myData[i,]$MagnetData1,myData[i,]$MagnetData2)
    angle = computeAngle(Fi[i,], mag)
    computeFn = absVector1(Fi[i,])^2 + Fc^2 - 2*Fc *absVector1(Fi[i,])*cos(angle)
    computeFn = sqrt(computeFn)
    computeFnData = c(computeFnData, computeFn)
  }
  print("computeFnData:")
  print(summary(computeFnData))
  
  initRotationMatrix = FiResults[[2]]
  a = (cosAlpha = cor(Fc, Fi[,1]))
  b = (cosBeta  = cor(Fc, Fi[,2]))
  c = (cosGama  = cor(Fc, Fi[,3]))
  print(paste(tag,"Cor_PC_Pi",round(a,3),round(b,3),round(c,3)))
  matplot(Fi, type="l", main = paste(tag,"Fi"), cex.main = 1.5, cex.lab = 1.5)
  legend("topright",c("x", "y", "z"),lty=1:3,col = 1:3)
  
  # computeProjOnHori(lv)
  # a^2 + b^2 +c^2
  
  vectorOnHori = computeProjOnHoriSingleData(c(a,b,c), cbind(firstData$Gravity0,firstData$Gravity1,firstData$Gravity2))
  
  angleBetweenFcAndLocal = computeAngle(vectorOnHori,initRotationMatrix[,2])
  return(angleBetweenFcAndLocal)
}

v = c(1,2,3);absv = absVector1(v)
Fc = computeFc()
Fcx = Fc/absv*v[1]; corX = cor(Fc,Fcx)
Fcy = Fc/absv*v[2]; corY = cor(Fc,Fcy)
Fcz = Fc/absv*v[3]; corZ = cor(Fc,Fcz)




computeAngleByPCA_COR(top,"top")
computeAngleByPCA_COR(leftHand,"leftHand")
computeAngleByPCA_COR(rightHand,"rightHand")
computeAngleByPCA_COR(leftPants,"leftPants")
computeAngleByPCA_COR(rightPants,"rightPants")


