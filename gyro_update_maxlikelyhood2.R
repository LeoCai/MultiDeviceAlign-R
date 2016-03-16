source("utils.R")
source("time_align_3_9.R")
source("getMatrixByMag.R")
source("gyroUpdateFunction.R")
library(zoo)
library(signal)

path = 2
i = 2
window = 30

smallOrLarge = "small"
fileParent = "3_10_data"

corResults1 = matrix(nrow=24,ncol = 6)
corResults2 = matrix(nrow=24,ncol = 6)
corResults3 = matrix(nrow=24,ncol = 6)

driftResults = c()

addResults = function(results,path,i,deviceNum){
  cors = results[[1]]; 
  corResults1[(path-1)*8+i,deviceNum] <<- cors[1];
  corResults2[(path-1)*8+i,deviceNum] <<- cors[2];
  corResults3[(path-1)*8+i,deviceNum] <<- cors[3];
  for(i in 2:7){
    driftResults <<- cbind(driftResults, results[[i]])
  }
  deviceNum <<- deviceNum + 1
}

gyroConvert = function(mdata,tag = "", lowpass=T, gyrpercent = 0.9){
  driftVerticalArray <<- c(); driftMagArray <<- c(); driftVertical = 0;driftMag = 0;
  driftVerticalArray_Carlibrated <<- c(); driftMagArray_Carlibrated <<- c();driftVertical_Carlibrated = 0;driftMag_Carlibrated = 0;
  driftVerticalArray_Complementary1<<- c(); driftMagArray_Complementary1 <<- c();driftVertical_Complementary1 = 0;driftMag_Complementary1 = 0;
  
  firstData= mdata[1,]
  initGV = cbind(firstData$Gravity0,firstData$Gravity1,firstData$Gravity2)
  initMagV = cbind(firstData$MagnetData0,firstData$MagnetData1,firstData$MagnetData2)
  initMatric = t(getMatrixByMag(initGV, initMagV))
  cuMatrix = initMatric
  cuMatrixCarlibrated = initMatric
  preMatrixCarlibrated = initMatric
  cuMatrix__complementary1 = initMatric
  initGVPre = initGV
  
  ucbAccs_Gyr = c()
  carlibratedAccs = c()
  cbAccs_complementary1 = c()
  
   preAccPitch = atan2(initGV[1], initGV[3])
   preAccRoll = atan2(initGV[2], initGV[3])
  
  
  for(i in 2:nrow(mdata)){
    sdata = mdata[i,];lacc = cbind(sdata$LinearAcc0,sdata$LinearAcc1,sdata$LinearAcc2);mag = cbind(sdata$MagnetData0,sdata$MagnetData1,sdata$MagnetData2)
    gyr = cbind(sdata$Gyro0, sdata$Gyro1, sdata$Gyro2);gacc = cbind(sdata$Gravity0, sdata$Gravity1, sdata$Gravity2);dt = sdata$dt
    
    cuMatrix = getUpdateMatrix(cuMatrix, gyr, dt)
    cuMatrixCarlibrated = getUpdateMatrix(cuMatrixCarlibrated, gyr, dt)
    
    # cuMatrix = getCarlibratedMatirx(gacc,rotationByMatrix(initGV,initMatric))
    #     acc_Acc = rotationByMatrix(lacc, accMatrix)
    #     acc_Accs = rbind(acc_Accs, acc_Acc)
#     print(paste("i",i))
#     print(cuMatrixCarlibrated)
    if(i%%100 == 0){
      accMatrix = getCarlibratedMatirx(gacc,initGVPre)
      cuMatrixCarlibrated = carlibrate(preMatrixCarlibrated%*%accMatrix,gacc,cuMatrixCarlibrated)
      initGVPre = gacc
      preMatrixCarlibrated = cuMatrixCarlibrated
    }
    
    gyroAcc = rotationByMatrix(lacc, cuMatrix)
    ucbAccs_Gyr = rbind(ucbAccs_Gyr, gyroAcc)
    
    carlibratedAcc = rotationByMatrix(lacc, cuMatrixCarlibrated)
    carlibratedAccs = rbind(carlibratedAccs, carlibratedAcc)
    
#     accMatrix = getCarlibratedMatirx(gacc, rotationByMatrix(initGV,initMatric))
# #     cuMatrix__complementary1 = accMatrix*0.05+cuMatrix__*0.95
# #     v2 = cuMatrix__complementary1[,2]
# #     v3 = gacc*0.05 + rotationByMatrix(rotationByMatrix(initGV,initMatric),t(cuMatrix__complementary1))*0.95
# #     # v2 = unitVector(cuMatrix__complementary1[,2]);v3 = unitVector(cuMatrix__complementary1[,3])
# #     v1 = crossProduct(v2, v3); v1 = unitVector(v1)
# #     cuMatrix__complementary1[,1] = v1; cuMatrix__complementary1[,2] = v2; cuMatrix__complementary1[,3] = v3
# #     acc_complementary1 = rotationByMatrix(lacc, cuMatrix__complementary1)
#     cbAccs_complementary1 = rbind(cbAccs_complementary1,rotationByMatrix(lacc,accMatrix)*0.3 + rotationByMatrix(lacc, cuMatrix)*0.7)
    
    cuAccPitch = atan2(gacc[1], gacc[3]); cuAccRoll = atan2(gacc[2], gacc[3])
    accDeltaPitch = cuAccPitch - preAccPitch; accDeltaRoll = cuAccRoll - preAccRoll
    preAccPitch = cuAccPitch; preAccRoll = cuAccRoll
    gyrComplementary = cbind(gyrpercent*gyr[1] + (1-gyrpercent)*accDeltaPitch, gyrpercent*gyr[2] + (1-gyrpercent)*accDeltaRoll, gyr[3])
    cuMatrix__complementary1 = getUpdateMatrix(cuMatrix__complementary1, gyrComplementary, dt)
    acc_complementary1 = rotationByMatrix(lacc, cuMatrix__complementary1)
    cbAccs_complementary1 = rbind(cbAccs_complementary1,acc_complementary1)
    
    driftVertical = computeAngle(rotationByMatrix(gacc,cuMatrix),rotationByMatrix(initGV,initMatric));driftMag = computeAngle(rotationByMatrix(mag,cuMatrix),rotationByMatrix(initMagV,initMatric))
    driftVerticalArray <<- c(driftVerticalArray, driftVertical);driftMagArray <<- c(driftMagArray, driftMag)
    
    driftVertical_Carlibrated =  computeAngle(rotationByMatrix(gacc,cuMatrixCarlibrated),rotationByMatrix(initGV,initMatric));driftMag_Carlibrated = computeAngle(rotationByMatrix(mag,cuMatrixCarlibrated),rotationByMatrix(initMagV,initMatric))
    driftVerticalArray_Carlibrated <<- c(driftVerticalArray_Carlibrated, driftVertical_Carlibrated);driftMagArray_Carlibrated <<- c(driftMagArray_Carlibrated, driftMag_Carlibrated)
    
    driftVertical_Complementary1 =  computeAngle(rotationByMatrix(gacc,cuMatrix__complementary1),rotationByMatrix(initGV,initMatric));driftMag_Complementary1 = computeAngle(rotationByMatrix(mag,cuMatrix__complementary1),rotationByMatrix(initMagV,initMatric))
    driftVerticalArray_Complementary1 <<- c(driftVerticalArray_Complementary1, driftVertical_Complementary1);driftMagArray_Complementary1 <<- c(driftMagArray_Complementary1, driftMag_Complementary1)
    
    
  }
  
  ucbAccs_Gyr = rbind(c(firstData$LinearAcc0,firstData$LinearAcc1,firstData$LinearAcc2),ucbAccs_Gyr)
  carlibratedAccs = rbind(c(firstData$LinearAcc0,firstData$LinearAcc1,firstData$LinearAcc2),carlibratedAccs)
  cbAccs_complementary1 = rbind(c(firstData$LinearAcc0,firstData$LinearAcc1,firstData$LinearAcc2),cbAccs_complementary1)
  
  magAccs = getGlobalAccByMag(mdata)
  
  cor_uncarlibrated = mean(c(cor(magAccs[,1],ucbAccs_Gyr[,1]),cor(magAccs[,2],ucbAccs_Gyr[,2]),cor(magAccs[,3],ucbAccs_Gyr[,3])))
  cor_carlibrated = mean(c(cor(magAccs[,1],carlibratedAccs[,1]),cor(magAccs[,2],carlibratedAccs[,2]),cor(magAccs[,3],carlibratedAccs[,3])))
  cor_complementary1 = mean(c(cor(magAccs[,1],cbAccs_complementary1[,1]),cor(magAccs[,2],cbAccs_complementary1[,2]),cor(magAccs[,3],cbAccs_complementary1[,3])))
  
#   print(paste(tag,"Only Gyrocope","cor",cor_uncarlibrated,"driftVertical",driftVertical,"driftMag",driftMag))
#   print(paste(tag,"Carlibrated","cor",cor_carlibrated,"driftVertical",driftVertical_Carlibrated,"driftMag",driftMag_Carlibrated))
#   print(paste(tag,"Complementary1","cor",cor_complementary1,"driftVertical",driftVertical_Complementary1,"driftMag",driftMag_Complementary1))
#   
#   matplot(cbind(driftVerticalArray,driftVerticalArray_Complementary1,driftVerticalArray_Carlibrated),type="l",main = paste(tag,"vertical drift"),lwd = 1.5,ylim = c(0,150))
#   legend("topright",c("only gyroscope","complementary1","carlibrated"),col=1:3,lty = 1:3)
#   
#   matplot(cbind(driftMagArray,driftMagArray_Complementary1,driftMagArray_Carlibrated),type="l",main = paste(tag,"mag drift"),lwd = 1.5,ylim = c(0,150))
#   legend("topright",c("only gyroscope","complementary1","carlibrated"),col=1:3,lty = 1:3)
#   
  return (list(c(cor_uncarlibrated,cor_complementary1,cor_carlibrated),
               driftVerticalArray,driftVerticalArray_Complementary1,driftVerticalArray_Carlibrated,
               driftMagArray,driftMagArray_Complementary1,driftMagArray_Carlibrated
               ))
  
  #   plot(driftVerticalArray,type = "l",main = paste(tag,"angle gravity drift"), 
  #        ylab = "angle gravity drift (in degree)", cex.main=1.5, cex.lab=1.5,ylim = c(0,150))
  #   plot(driftMagArray,type = "l",main = paste(tag,"angle magnet drift"), 
  #        ylab = "angle magnet drift (in degree)", cex.main=1.5, cex.lab=1.5,ylim = c(0,150))
}


for(smallOrLarge in c("large")){
  for(window in c(10, 30, 60)){
    s = 101;e = 100+window*50
   # selectIndex <<- 1:(50*window)
    
    if(smallOrLarge == "large") {
      pathIndex <<- 4:6
    }else{
      pathIndex <<- 1:3
    }
    for(path in pathIndex){
      for(i in 1:8){
        deviceNum <<- 1
        print(paste(smallOrLarge,"window:",window,"path",path,"i",i,"--------------------------------"))
        readSample(path, i, s, e)
        if(smallOrLarge == "large") {
          rowIndex = path-3 
        }
        else {
          rowIndex = path
        }
        results = gyroConvert(top,"TOP",T,gyrpercent = 0.9); addResults(results,rowIndex,i,deviceNum)
        results = gyroConvert(rightHand,"RIGHTHAND",T,gyrpercent = 0.9); addResults(results,rowIndex,i,deviceNum)
        results = gyroConvert(leftHand,"LEFTHAND",T,gyrpercent = 0.9); addResults(results,rowIndex,i,deviceNum)
        results = gyroConvert(leftPants,"LEFTPANTS",T,gyrpercent = 0.9); addResults(results,rowIndex,i,deviceNum)
        results = gyroConvert(rightPants,"RIGHTPANTS",T,gyrpercent = 0.9); addResults(results,rowIndex,i,deviceNum)
        results = gyroConvert(glass,"GLASS",T,gyrpercent = 0.9); addResults(results,rowIndex,i,deviceNum)
      }
    }
    # write.csv(driftResults,paste("./summary_gyro_tracking/drifts_",smallOrLarge,"_",window,".csv",sep = ""),row.names = F)
    write.csv(corResults1,paste("./summary_gyro_tracking/cor_gyro_",smallOrLarge,"_",window,".csv",sep = ""),row.names = F)
    write.csv(corResults2,paste("./summary_gyro_tracking/cor_complementary_",smallOrLarge,"_",window,".csv",sep = ""),row.names = F)
    write.csv(corResults3,paste("./summary_gyro_tracking/cor_calibration_",smallOrLarge,"_",window,".csv",sep = ""),row.names = F)
    corResults1
    corResults2
    corResults3
    driftResults
  }
}







