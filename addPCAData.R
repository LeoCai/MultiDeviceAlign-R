pcaData = c()

addPCAData = function(magnitude2D){
  pcaData <<- cbind(pcaData, magnitude2D)
}

addDeviceDataToPCA2 = function(myData, tag= ""){
  firstData = myData[1,]
  gvInit = cbind(firstData$Gravity0,firstData$Gravity1,firstData$Gravity2)
  mgInit = cbind(firstData$MagnetData0,firstData$MagnetData1,firstData$MagnetData2)
  initRotationMatirx = getMatrixByMag(gvInit,mgInit)
  globalAccs = getGlobalAccByMag(myData)
  globalAccs[,3] = 0; #proj on horizontal
  # matplot(globalAccs,type = "l",col = c(1,2,3),main = "global accs")
  rotatedGlobal = c()
  for (i in 1:nrow(globalAccs)) {
#     temp = initRotationMatirx %*% matrix(globalAccs[i,],nrow = 3,byrow = F)
#     rotatedAcc = c(temp[1,1],temp[2,1],temp[3,1])
    rotatedAcc = rotationByMatrix(globalAccs[i,],initRotationMatirx)
    # rotatedAccOnHori = computeProjOnHoriSingleData(rotatedAcc,gvInit)
    rotatedGlobal = rbind(rotatedGlobal, rotatedAcc)
  }
  for(i in 1:3){
    addPCAData(rotatedGlobal[,i])
  }
}

addDeviceDataToPCA = function(deviceData, tag= ""){
  pHData = c()
  for(i in 1:nrow(deviceData)){
    singleData = deviceData[i,]
    lv = cbind(singleData$LinearAcc0,singleData$LinearAcc1,singleData$LinearAcc2)
    gv = cbind(singleData$Gravity0,singleData$Gravity1,singleData$Gravity2)
    # lv2 = cbind(singleData$lv1,singleData$lv2,singleData$lv3)
    # gv2 = cbind(singleData$gv1,singleData$gv2,singleData$gv3)
    # print(paste(lv-lv2,gv-gv2))
    pH = computeProjOnHoriSingleData(lv,gv)
    pHData = c(pHData, absVector1(pH))
  }
  # plot(pHData, type="l", main= paste(tag, "projection on horizontal"),ylab = "acc(m/s^2)", cex.lab=1.5,cex.main=1.5)
  addPCAData(pHData)
}

computeFc = function() {
  pcaData <<- c()
  addDeviceDataToPCA(top,"top")
  addDeviceDataToPCA(leftHand,"leftHand")
  addDeviceDataToPCA(rightHand,"rightHand")
  addDeviceDataToPCA(rightPants,"rightPants")
  addDeviceDataToPCA(leftPants,"leftPants")
  addDeviceDataToPCA(glass,"glass")
  # write.csv(pcaData,"./summary_fi_fc_pca_angle_cor/pcaData.csv",row.names = F)
  epca6A <- prcomp(pcaData,
                   center = F,
                   scale. = F)
  print(epca6A$sdev^2)
  # plot(epca6A$sdev,type = "l" ,main = "egvalue")
  newData = predict(epca6A, newdata = pcaData)
  pcaForward = -newData[selectIndex,1]
  if(abs(min(pcaForward))<abs(max(pcaForward))) pcaForward = -pcaForward
  plot(pcaForward[1:150],type="l",main="Fc",ylim = c(-2,2))
  return(pcaForward)
}

