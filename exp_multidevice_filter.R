source("preprocess.R")
source("../handshake_9_11/model/GloableConvert.R")
source("./utils.R")
source("./readData.R")
source("./smartfilterFunction.R")

projHori = function(lv, gv){
  absProj = ((
    lv[1] * gv[1] + lv[2] * gv[2] + lv[3] * gv[3]
  ) /
    absVector(gv[1], gv[2], gv[3])
  )
  gravityUnitVector = gv/absVector(gv[1],gv[2],gv[3])
  gravityProj = gravityUnitVector*absProj
  horiProj = lv - gravityProj
  return(horiProj)
}

rotateByAngle = function(v, angle){
  #ni shi zhen rotattion
  angle = angle/180*pi
  rtm = c()
  rtm = rbind(rtm, c(cos(angle), -sin(angle), 0))
  rtm = rbind(rtm, c(sin(angle), cos(angle), 0))
  rtm = rbind(rtm, c(0, 0, 1))
  rtm = as.matrix(rtm)
  (vmatrix = matrix(data = v, byrow = F))
  vector = rtm%*%vmatrix
  return(c(vector[1,1],vector[2,1],vector[3,1]))
}

findAngleByAcc = function(data, angle,th1, th2){
  globalAcc = cbind(data$ConvertedData0,data$ConvertedData1,data$ConvertedData2)
  rotatedGlobalAcc = data.frame()
  for(i in 1:nrow(globalAcc)){
    rotatedGlobalAcc = rbind(rotatedGlobalAcc,rotateByAngle(globalAcc[i,], angle))
  }
  
  # plot(globalAcc[,1],type = "l", main = "global left right" )
  # plot(globalAcc[,2],type = "l", main = "global forward")
  
  mean(globalAcc[,1])
  mean(globalAcc[,2])
  
  filted_global_leftright = smartFilter(globalAcc[,1],maxThrehold = th1, tag="global left right",mylim = c(-1.3,1.3))
  filted_global_forward= smartFilter(globalAcc[,2],maxThrehold = th2, tag="global forward",mylim = c(-4,2))
  mean1_global = mean(filted_global_leftright)
  mean2_global = mean(filted_global_forward)
  print("avg angle")
  print(paste(mean1_global,mean2_global))
  print(abs(atan(mean1_global/mean2_global))/pi*180)
  
  
  # plot(rotatedGlobalAcc[,1],type = "l", main = "rotated v1")
  # plot(rotatedGlobalAcc[,2],type = "l", main = "roteted v2")
  
#   filted_rotated_leftright = smartFilter(rotatedGlobalAcc[,1],maxThrehold = 100, tag="global left right",mylim = c(-2,3))
#   filted_rotated_forward= smartFilter(rotatedGlobalAcc[,2],maxThrehold = 100, tag="global forward",mylim = c(-4,2))
#   mean1 = mean(filted_rotated_leftright)
#   mean2 = mean(filted_rotated_forward)
#   return (list(angle = abs(atan(mean1/mean2))/pi*180, xmean =  mean1, ymean = mean2))
}

testAnglesByAcc = function(d, tag,th1 = 1,th2 = 1){
  results = data.frame()
  # names(results) = c()
  for(angle in 45){
    computedAngle = findAngleByAcc(d, angle,th1 = 1,th2 = 1)
    # results = rbind(results, c(angle,computedAngle$angle, computedAngle$xmean, computedAngle$ymean))
    # print(c(angle,computedAngle$angle))
  }
  # names(results) = c("realAngle","computedAngle","xmean","ymean")
  # write.csv(results,paste("./results_multidevices_angles_test_car/",tag,"_testAnglesByAcc.csv",sep = ""))
#   matplot(x = 1:nrow(results), y=results[,1:2] , type ="l", cex.main=1.5,cex.lab=1.5, main = paste(tag,"compute angle by average"), ylab = "angle in degree", xlab= "index")
#   legend(
#     "topright",c("real angle", "computed angle by acc"),lty = 1:2, col = 1:2,cex = 1.5
#   )
}

testAnglesByAcc(top[0:300,], "top")
testAnglesByAcc(leftHand[33:275,], "leftHand",th1 = 1,th2 = 1)
testAnglesByAcc(rightHand[0:300,], "rightHand")
testAnglesByAcc(leftPants[0:300,], "leftPants")
testAnglesByAcc(rightPants[0:300,], "rightPants")

# plot3d(globalAcc,type = "l")
# lines3d(rotatedGlobalAcc,col="red")


lvTop = cbind(top$ConvertedData0,top$ConvertedData1,top$ConvertedData2)
plot(lvTop[0:300,3],type = "l")


lvTop = cbind(leftHand$ConvertedData0,leftHand$ConvertedData1,leftHand$ConvertedData2)
plot(lvTop[0:300,3],type = "l")


lvTop = cbind(rightHand$ConvertedData0,rightHand$ConvertedData1,rightHand$ConvertedData2)
plot(lvTop[0:300,3],type = "l")


lvTop = cbind(leftPants$ConvertedData0,leftPants$ConvertedData1,leftPants$ConvertedData2)
plot(lvTop[0:300,3],type = "l")


lvTop = cbind(rightPants$ConvertedData0,rightPants$ConvertedData1,rightPants$ConvertedData2)
plot(lvTop[0:300,3],type = "l")
