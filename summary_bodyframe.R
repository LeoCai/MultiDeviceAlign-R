source("./utils.R")
source("./timealign_3_2.R")
source("./getMatrixByMag.R")
source("./addPCAData.R")
source("./plot_debug.R")
library(zoo)

# globalacc = getGlobalAccByMag(top)
# lowpasstop = rollapply(top,5,mean)
# lowpasstop = as.data.frame(lowpasstop)
# globalacclp = getGlobalAccByMag(lowpasstop)
# matplot(cbind(globalacc[1:300,2],globalacclp[1:300,2]),type="l")
# plot(globalacc[,2],type = "l")

selectIndex = 1:200
s1 = glass[selectIndex,]
s2 = top[selectIndex,]
s3 = leftHand[selectIndex,]
s4 = rightHand[selectIndex,]
s5 = leftPants[selectIndex,]
s6 = rightPants[selectIndex,]

xlocal = c()
addX = function(mData){
  xlocal<<-cbind(xlocal,mData$LinearAcc0)
}
ylocal = c()
addY = function(mData){
  ylocal<<-cbind(ylocal,mData$LinearAcc1)
}

zlocal = c()
addZ = function(mData){
  zlocal<<-cbind(zlocal,mData$LinearAcc2)
}
magnitudeLocal = c()
addMagnitude = function(mData){
  magnitudeLocal<<-cbind(magnitudeLocal,sqrt(mData$LinearAcc0^2+mData$LinearAcc1^2+mData$LinearAcc2^2))
}

addX(s1)
addX(s2)
addX(s3)
addX(s4)
addX(s5)
addX(s6)

matplot(xlocal,type="l")

addY(s1)
addY(s2)
addY(s3)
addY(s4)
addY(s5)
addY(s6)

addZ(s1)
addZ(s2)
addZ(s3)
addZ(s4)
addZ(s5)
addZ(s6)

addMagnitude(s1)
addMagnitude(s2)
addMagnitude(s3)
addMagnitude(s4)
addMagnitude(s5)
addMagnitude(s6)

matplot(magnitudeLocal,type="l")

write.csv(xlocal,"./summary_bodyframe_results/xlocal.csv",row.names = F)
write.csv(ylocal,"./summary_bodyframe_results/ylocal.csv",row.names = F)
write.csv(zlocal,"./summary_bodyframe_results/zlocal.csv",row.names = F)
write.csv(magnitudeLocal,"./summary_bodyframe_results/magnitudeLocal.csv",row.names = F)



