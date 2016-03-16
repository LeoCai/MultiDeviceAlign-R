source("utils.R")
source("timealign_gyro_normalwalk_3_6.R")
source("getMatrixByMag.R")
library(zoo)
library(signal)


getUpdateMatrix = function(cuMatrix, gyr, dt) {
  #wait for test
  delta = sqrt((gyr[1] * dt) ^ 2 + (gyr[2] * dt) ^ 2 + (gyr[3] * dt) ^ 2)
  B = matrix(
    c(
      0,             -gyr[3] * dt,    gyr[2] * dt,
      gyr[3] * dt,   0,               -gyr[1] * dt,
      -gyr[2] * dt,  gyr[1] * dt,     0
    ),
    nrow = 3, ncol = 3, byrow = TRUE
  )
  B1 = B * (sin(delta) / delta)
  B2 = (1 - cos(delta)) / (delta ^ 2) * (B %*% B)
  updateMatrix = I + B1 + B2
  return(cuMatrix%*%updateMatrix)
}

RotationMatrix = function(angle, u){
  norm = absVector1(u)
  u[1] = u[1] / norm; u[2] = u[2] / norm; u[3] = u[3] / norm 
  rotationMatrix = matrix(nrow = 3, ncol = 3)
  rotationMatrix[1,1] = cos(angle) + u[1]^2*(1-cos(angle))
  rotationMatrix[1,2] = u[1]*u[2]*(1-cos(angle)) - u[3]*sin(angle)
  rotationMatrix[1,3] = u[2]*sin(angle) + u[1]*u[3]*(1-cos(angle))
  
  rotationMatrix[2,1] = u[3]*sin(angle) + u[1]*u[2]*(1-cos(angle))
  rotationMatrix[2,2] = cos(angle) + u[2]^2 * (1-cos(angle))
  rotationMatrix[2,3] = -u[1]*sin(angle) + u[2]*u[3]*(1-cos(angle))
  
  rotationMatrix[3,1] = -u[2]*sin(angle) + u[1]*u[3]*(1-cos(angle))
  rotationMatrix[3,2] = u[1]*sin(angle) + u[2]*u[3]*(1-cos(angle))
  rotationMatrix[3,3] = cos(angle) + u[3]^2*(1-cos(angle))
  
  return(rotationMatrix)
}

getCarlibratedMatirx = function(computedInitGV, initRealGV){
  vectorBefore = computedInitGV; vectorAfter = initRealGV
  rotationAxis = crossProduct(vectorBefore, vectorAfter)
  rotationAngle = acos(dotProduct(vectorBefore, vectorAfter) / absVector1(vectorBefore)/absVector1(vectorAfter))
  # rotationAngles <<- c(rotationAngles, rotationAngle/pi*180)
  # print(paste("rotationAngle",rotationAngle/pi*180))
  rotationMatrix = RotationMatrix(rotationAngle, rotationAxis)
  return(rotationMatrix)
}

carlibrate = function(rm2,rotationAxis, gyromatrix){
  mindist = 10000000;  bestAngle = 0;  bestRm = 0
  
  for(angle in 0:360){
    rm = RotationMatrix(angle/pi*180, rotationAxis);dist = mean((rm2%*%rm - gyromatrix)^2)
    if(dist<mindist){mindist = dist; bestAngle = angle;bestRm = rm2%*%rm}
    # print(paste(dist,angle))
  }
  # print(paste(mindist,bestAngle))
  return (bestRm)
}

# vectorBefore = c(1,3,5); vectorAfter = c(1,1,1)
# vectorBefore = unitVector(vectorBefore); vectorAfter = unitVector(vectorAfter)
# rm = getUpdateMatirx2(vectorBefore, vectorAfter)
# rm%*%matrix(vectorBefore,nrow = 3)


matplot(cbind(top$MagnetData1,lowFilter(top$MagnetData1)),main="low pass",type="l")

gyroConvert = function(mdata,tag = "", lowpass=T, gyrpercent = 0.9){
  driftVerticalArray <<- c(); driftMagArray <<- c();driftVertical = 0;driftMag = 0;
  driftVerticalArray_Carlibrated <<- c(); driftMagArray_Carlibrated <<- c();driftVertical_Carlibrated = 0;driftMag_Carlibrated = 0;
  
  firstData= mdata[1,]
  initGV = cbind(firstData$Gravity0,firstData$Gravity1,firstData$Gravity2)
  initMagV = cbind(firstData$MagnetData0,firstData$MagnetData1,firstData$MagnetData2)
  initMatric = t(getMatrixByMag(initGV, initMagV))
  cuMatrix = initMatric
  cuMatrixCarlibrated = initMatric
  
  ucbAccs_Gyr = c()
  carlibratedAccs = c()
  
  for(i in 2:nrow(mdata)){
    sdata = mdata[i,];lacc = cbind(sdata$LinearAcc0,sdata$LinearAcc1,sdata$LinearAcc2);mag = cbind(sdata$MagnetData0,sdata$MagnetData1,sdata$MagnetData2)
    gyr = cbind(sdata$Gyro0, sdata$Gyro1, sdata$Gyro2);gacc = cbind(sdata$Gravity0, sdata$Gravity1, sdata$Gravity2);dt = sdata$dt
    
    cuMatrix = getUpdateMatrix(cuMatrix, gyr, dt)
    cuMatrixCarlibrated = getUpdateMatrix(cuMatrixCarlibrated, gyr, dt)
    
    # cuMatrix = getCarlibratedMatirx(gacc,rotationByMatrix(initGV,initMatric))
#     acc_Acc = rotationByMatrix(lacc, accMatrix)
#     acc_Accs = rbind(acc_Accs, acc_Acc)
    
    if(i%%100 == 0){
      accMatrix = getCarlibratedMatirx(gacc,rotationByMatrix(initGV,initMatric))
      cuMatrixCarlibrated = carlibrate(accMatrix,gacc,cuMatrixCarlibrated)
    }
    
    gyroAcc = rotationByMatrix(lacc, cuMatrix)
    ucbAccs_Gyr = rbind(ucbAccs_Gyr, gyroAcc)
    
    carlibratedAcc = rotationByMatrix(lacc, cuMatrixCarlibrated)
    carlibratedAccs = rbind(carlibratedAccs, carlibratedAcc)
    
    driftVertical = computeAngle(rotationByMatrix(gacc,cuMatrix),rotationByMatrix(initGV,initMatric));driftMag = computeAngle(rotationByMatrix(mag,cuMatrix),rotationByMatrix(initMagV,initMatric))
    driftVerticalArray <<- c(driftVerticalArray, driftVertical);driftMagArray <<- c(driftMagArray, driftMag)
    
    driftVertical_Carlibrated =  computeAngle(rotationByMatrix(gacc,cuMatrixCarlibrated),rotationByMatrix(initGV,initMatric));driftMag_Carlibrated = computeAngle(rotationByMatrix(mag,cuMatrixCarlibrated),rotationByMatrix(initMagV,initMatric))
    driftVerticalArray_Carlibrated <<- c(driftVerticalArray_Carlibrated, driftVertical_Carlibrated);driftMagArray_Carlibrated <<- c(driftMagArray_Carlibrated, driftMag_Carlibrated)
  }
  
  ucbAccs_Gyr = rbind(c(firstData$LinearAcc0,firstData$LinearAcc1,firstData$LinearAcc2),ucbAccs_Gyr)
  carlibratedAccs = rbind(c(firstData$LinearAcc0,firstData$LinearAcc1,firstData$LinearAcc2),carlibratedAccs)
  
  magAccs = getGlobalAccByMag(mdata)
  
  cor_uncarlibrated = mean(c(cor(magAccs[,1],ucbAccs_Gyr[,1]),cor(magAccs[,2],ucbAccs_Gyr[,2]),cor(magAccs[,3],ucbAccs_Gyr[,3])))
  cor_carlibrated = mean(c(cor(magAccs[,1],carlibratedAccs[,1]),cor(magAccs[,2],carlibratedAccs[,2]),cor(magAccs[,3],carlibratedAccs[,3])))
  
  print(paste(tag,"Only Gyrocope","cor",cor_uncarlibrated,"driftVertical",driftVertical,"driftMag",driftMag))
  print(paste(tag,"Only Gyrocope","cor",cor_carlibrated,"driftVertical",driftVertical_Carlibrated,"driftMag",driftMag_Carlibrated))
  
  matplot(cbind(driftVerticalArray,driftVerticalArray_Carlibrated),type="l",main = paste(tag,"vertical drift"),lwd = 1.5)
  legend("topright",c("only gyroscope","carlibrated"),col=1:2,lty = 1:2)
  
  matplot(cbind(driftMagArray,driftMagArray_Carlibrated),type="l",main = paste(tag,"mag drift"),lwd = 1.5)
  legend("topright",c("only gyroscope","carlibrated"),col=1:2,lty = 1:2)
  
#   plot(driftVerticalArray,type = "l",main = paste(tag,"angle gravity drift"), 
#        ylab = "angle gravity drift (in degree)", cex.main=1.5, cex.lab=1.5,ylim = c(0,150))
#   plot(driftMagArray,type = "l",main = paste(tag,"angle magnet drift"), 
#        ylab = "angle magnet drift (in degree)", cex.main=1.5, cex.lab=1.5,ylim = c(0,150))
}

gyroConvert(top,"TOP",T,gyrpercent = 0.90)
gyroConvert(rightHand,"RIGHTHAND",T,gyrpercent = 0.90)
gyroConvert(leftHand,"LEFTHAND",T,gyrpercent = 0.90)
gyroConvert(leftPants,"LEFTPANTS",T,gyrpercent = 0.90)
gyroConvert(rightPants,"RIGHTPANTS",T,gyrpercent = 0.90)
gyroConvert(glass,"GLASS",T,gyrpercent = 0.90)





