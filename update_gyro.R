source("utils.R")
source("timealign_gyro_3_6.R")
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
  rotationAngles <<- c(rotationAngles, rotationAngle/pi*180)
  # print(paste("rotationAngle",rotationAngle/pi*180))
  rotationMatrix = RotationMatrix(rotationAngle, rotationAxis)
  return(rotationMatrix)
}

# vectorBefore = c(1,3,5); vectorAfter = c(1,1,1)
# vectorBefore = unitVector(vectorBefore); vectorAfter = unitVector(vectorAfter)
# rm = getUpdateMatirx2(vectorBefore, vectorAfter)
# rm%*%matrix(vectorBefore,nrow = 3)


matplot(cbind(top$MagnetData1,lowFilter(top$MagnetData1)),main="low pass",type="l")

gyroConvert = function(mdata,tag = "", lowpass=T, gyrpercent = 0.9){
  rotationAngles <<- c()
  firstData= mdata[1,]
  initGV = cbind(firstData$Gravity0,firstData$Gravity1,firstData$Gravity2)
  initMagV = cbind(firstData$MagnetData0,firstData$MagnetData1,firstData$MagnetData2)
  initMatric = t(getMatrixByMag(initGV, initMagV))
  cuMatrix = initMatric
  cuMatrixComplementary = initMatric
  
  ucbAccs_Gyr = c()
  ucbAccs_Acc = c()
  cbAccs_complementary = c()
  cbAccs = c()
  globalAccsComplementary = c()
  
  if(lowpass){
    mdata$Gravity0 = lowFilter(mdata$Gravity0)
    mdata$Gravity1 = lowFilter(mdata$Gravity1)
    mdata$Gravity2 = lowFilter(mdata$Gravity2)
  }
  
  preAccRoll = atan2(initGV[1], initGV[3])
  preAccPitch = atan2(initGV[2], initGV[3])
  preAccYaw = atan2(initGV[1], initGV[2])
  
  for(i in 2:nrow(mdata)){
    sdata = mdata[i,]
    lacc = cbind(sdata$LinearAcc0,sdata$LinearAcc1,sdata$LinearAcc2)
    gyr = cbind(sdata$Gyro0, sdata$Gyro1, sdata$Gyro2)
    gacc = cbind(sdata$Gravity0, sdata$Gravity1, sdata$Gravity2)
   
    dt = sdata$dt
    cuMatrix = getUpdateMatrix(cuMatrix, gyr, dt)
    unCarliratedGlobalAcc = rotationByMatrix(lacc, cuMatrix)
    ucbAccs_Gyr = rbind(ucbAccs_Gyr, unCarliratedGlobalAcc)
    
    cuAccPitch = atan2(gacc[1], gacc[3]); cuAccRoll = atan2(gacc[2], gacc[3]); cuAccYaw = atan2(gacc[1], gacc[2])
    accDeltaPitch = cuAccPitch - preAccPitch; accDeltaRoll = cuAccRoll - preAccRoll; accDeltaYaw = cuAccYaw - preAccYaw
    preAccPitch = cuAccPitch; preAccRoll = cuAccRoll; preAccYaw = cuAccYaw
    gyrComplementary = cbind(gyrpercent*gyr[1] + (1-gyrpercent)*accDeltaPitch, gyrpercent*gyr[2] + (1-gyrpercent)*accDeltaRoll, gyrpercent*gyr[3]+(1-gyrpercent)*accDeltaYaw)
    cuMatrixComplementary = getUpdateMatrix(cuMatrixComplementary, gyrComplementary, dt)
    globalAccComplementary = rotationByMatrix(lacc, cuMatrixComplementary)
    globalAccsComplementary = rbind(globalAccsComplementary, globalAccComplementary)
    
    computedInitGV = rotationByMatrix(gacc, cuMatrix)
    carlibratedMatrix = getCarlibratedMatirx(rotationByMatrix(initGV,initMatric), computedInitGV)
    carlibratedGlobalAcc = rotationByMatrix(unCarliratedGlobalAcc, carlibratedMatrix)
    cbAccs = rbind(cbAccs, carlibratedGlobalAcc)
    
    accMatrix = getCarlibratedMatirx(rotationByMatrix(initGV,initMatric), gacc)
    ucbAccBy_Acc = rotationByMatrix(lacc, accMatrix)
    ucbAccs_Acc  = rbind(ucbAccs_Acc, ucbAccBy_Acc)
#     
    cbAccs_complementary = rbind(cbAccs_complementary,as.vector(unCarliratedGlobalAcc)*0.9 - as.vector(ucbAccBy_Acc)*0.1)
  }
  
  ucbAccs_Gyr = rbind(c(firstData$LinearAcc0,firstData$LinearAcc1,firstData$LinearAcc2),ucbAccs_Gyr)
  cbAccs  = rbind(c(firstData$LinearAcc0,firstData$LinearAcc1,firstData$LinearAcc2),cbAccs)
  ucbAccs_Acc = rbind(c(firstData$LinearAcc0,firstData$LinearAcc1,firstData$LinearAcc2),ucbAccs_Acc)
  cbAccs_complementary = rbind(c(firstData$LinearAcc0,firstData$LinearAcc1,firstData$LinearAcc2),cbAccs_complementary)
  globalAccsComplementary = rbind(c(firstData$LinearAcc0,firstData$LinearAcc1,firstData$LinearAcc2),globalAccsComplementary)
  
  magAccs = getGlobalAccByMag(mdata)
  
  cor_uncarlibrated = mean(c(cor(magAccs[,1],ucbAccs_Gyr[,1]),cor(magAccs[,2],ucbAccs_Gyr[,2]),cor(magAccs[,3],ucbAccs_Gyr[,3])))
  cor_carlibrated = mean(c(cor(magAccs[,1],cbAccs[,1]),cor(magAccs[,2],cbAccs[,2]),cor(magAccs[,3],cbAccs[,3])))
  
  matplot(magAccs,type = "l" , main = paste(tag,"magAccs"),cex.main = 1.5, cex.lab=1.5,ylab = "magnitude(m/s^2)")
  legend(
    "topright",c("Right","Forward","Up"),lty = 1:3, col = 1:3,cex = 1.5
  )
  matplot(ucbAccs_Gyr,type = "l" , main = paste(tag,"uncarlibrated Accs Mean-Cor:",cor_uncarlibrated),cex.main = 1.5, cex.lab=1.5,ylab = "magnitude(m/s^2)")
  legend(
    "topright",c("Right","Forward","Up"),lty = 1:3, col = 1:3,cex = 1.5
  )
  matplot(cbAccs,type = "l" , main = paste(tag,"carlibrated Accs Mean-Cor:",cor_carlibrated),cex.main = 1.5, cex.lab=1.5,ylab = "magnitude(m/s^2)")
  legend(
    "topright",c("Right","Forward","Up"),lty = 1:3, col = 1:3,cex = 1.5
  )
  
  matplot(cbind(magAccs[,2],ucbAccs_Gyr[,2],ucbAccs_Acc[,2],cbAccs_complementary[,2],cbAccs[,2]),type = "l",main=paste(tag,"forward global acc compare") ,lwd = 1,cex.main=1.5,cex.lab= 1.5)
  legend("topright",c("mag","gyr","acc","complementary","carlibrate"),col = 1:4, lty=1:4)
  
  nrow(magAccs)
  
#   print("1")
#   print(cor(magAccs[,1],ucbAccs_Gyr[,1]))
#   print(cor(magAccs[,1],cbAccs[,1]))
#   
  dimentions = c("RIGHT","FORWARD","UP")
  for(i in 1:3){
    print(paste(tag,dimentions[i],"only by gyroscope    cor:",cor(magAccs[,i],ucbAccs_Gyr[,i])))
    print(paste(tag,dimentions[i],"lowpass",lowpass, "only by acc    cor:",cor(magAccs[,i],ucbAccs_Acc[,i])))
    print(paste(tag,dimentions[i],"caribrate by t0    cor:",cor(magAccs[,i],cbAccs[,i])))
    print(paste(tag,dimentions[i],"lowpass",lowpass,"complementary    cor:",cor(magAccs[,i],cbAccs_complementary[,i])))
    print(paste(tag,dimentions[i],"lowpass",lowpass,"input complementary    cor:",cor(magAccs[,i],globalAccsComplementary[,i])))
    
  }
 
#   
#   print("3")
#   print(cor(magAccs[,3],ucbAccs_Gyr[,3]))
#   print(cor(magAccs[,3],cbAccs[,3]))
  plot(rotationAngles,type = "l",main = paste(tag,"angle drift"), 
       ylab = "angle drift (in degree)", cex.main=1.5, cex.lab=1.5)
}

gyroConvert(top,"TOP",T,gyrpercent = 0.90)
gyroConvert(rightHand,"RIGHTHAND",T,gyrpercent = 0.90)
gyroConvert(leftHand,"LEFTHAND",T,gyrpercent = 0.90)
gyroConvert(leftPants,"LEFTPANTS",T,gyrpercent = 0.90)
gyroConvert(rightPants,"RIGHTPANTS",T,gyrpercent = 0.90)
gyroConvert(glass,"GLASS",T,gyrpercent = 0.90)





