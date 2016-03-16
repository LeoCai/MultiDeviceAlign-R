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

selectIndex = 1:699
s1 = getGlobalAccByMag(glass[selectIndex,])
s2 = getGlobalAccByMag(top[selectIndex,])
s3 = getGlobalAccByMag(leftHand[selectIndex,])
s4 = getGlobalAccByMag(rightHand[selectIndex,])
s5 = getGlobalAccByMag(leftPants[selectIndex,])
s6 = getGlobalAccByMag(rightPants[selectIndex,])

xglobal = c()
addX = function(mData){
  xglobal<<-cbind(xglobal,mData[,1])
}
yglobal = c()
addY = function(mData){
  yglobal<<-cbind(yglobal,mData[,2])
}

zglobal = c()
addZ = function(mData){
  zglobal<<-cbind(zglobal,mData[,3])
}

addX(s1)
addX(s2)
addX(s3)
addX(s4)
addX(s5)
addX(s6)

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

write.csv(xglobal,"./summary_globalframe/xglobal.csv",row.names = F)
write.csv(yglobal,"./summary_globalframe/yglobal.csv",row.names = F)
write.csv(zglobal,"./summary_globalframe/zglobal.csv",row.names = F)



