source("./utils.R")
source("./readData.R")
source("./getMatrixByMag.R")
source("./addPCAData.R")
source("./plot_debug.R")
library(zoo)

selectIndex = 200:500

addDeviceDataToPCA = addDeviceDataToPCA2

Fc = computeFc() 
mdata = top
tag = "top"

getFinNew = function(mdata,FcAngle = 15,tag="",plotFin = F){
  globalAcc = getGlobalAccByMag(mdata)[selectIndex,]
  globalAcc[,3] = 0
  Fi = globalAcc
  angle = (90-FcAngle)*pi/180
  FcVector = cbind(Fc*cos(angle),Fc*sin(angle),0)
  Fin = Fi- FcVector
  matplot(Fi,type="l",main=paste(tag,"Fi"),cex.main=1.5,cex.lab=1.5,ylab="magnitude")
  legend("topright",c("Right","Forward","Up"),col=1:3,lty=1:3)
  matplot(Fin,type="l",main=paste(tag,"Fin"),cex.main=1.5,cex.lab=1.5,ylab="magnitude")
  legend("topright",c("Right","Forward","Up"),col=1:3,lty=1:3)
  if(plotFin){
    init3d(tag)
    myArrow3d(c(cos(angle),sin(angle),0)*5,2)
    for(i in 1:nrow(Fin)){
      myArrow3d(Fin[i,])
    }
  }
  return(Fin)
}

getFinNew(top,15,"top",T)
getFinNew(leftHand,15,"leftHand",T)
getFinNew(rightHand,15,"rightHand",T)
getFinNew(leftPants,15,"leftPants",T)
getFinNew(rightPants,15,"rightPants",T)


