source("./utils.R")
source("./timealign_3_3_speedud.R")
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

selectIndex = 1:300

addDeviceDataToPCA = addDeviceDataToPCA2

gacctop = getGlobalAccByMag(top)

# plot(gacctop[selectIndex,2],type="l",main="top forward")
# cor(-Fc[selectIndex], gacctop[selectIndex,2])

Fc = computeFc() 
getAngleSimple = function(mdata, tag="", plot3d = F){
  globalAcc = getGlobalAccByMag(mdata)[selectIndex,]
  globalAcc[,3] = 0
  Fi = globalAcc
  tr = 0.01; results = c()
  dimStr = c("x","y","z")
  for (dim in 1:3) {
    # mean = 0;count = 0
    fiFc = c()
    
    for (i in 1:length(Fc)) {
      if (abs(Fc[i]) > tr) {
        fiFc = c(fiFc, Fi[i,dim] / Fc[i])
        # mean = mean + (Fi[i,dim] / abs(Fc[i])); count = count + 1
      }
    }
    meanR = 0
    if(length(fiFc)!=0) {
      # print(fiFc)
      sdFiFc = sd(fiFc)
      meanFiFc = mean(fiFc)
      fiFcfilted = fiFc[abs(fiFc)<2]
      meanR = mean(fiFcfilted)
      if(length(fiFcfilted)>0)
      plot(fiFcfilted, type="l", main=paste(tag,dimStr[dim],"fi/|fc| mean:",round(meanR,3)))
    }
    
    # if(count!=0) {mean = mean / count} else { mean = 0}
    results = c(results,meanR)
  }
  angle = computeAngle(results,c(0,1,0))
  sig = getDirection(results, c(0,1,0),c(0,0,1))
  print(tag)
  print(results)
  print(sig*angle)
  if(plot3d){
    init3d(tag)
    myArrow3d(results*4,2)
    for(i in 1:nrow(globalAcc)){
      myArrow3d(globalAcc[i,])
    }
  }
}
getAngleSimple(top, "top",F)
getAngleSimple(leftHand, "leftHand",F)
getAngleSimple(rightHand, "rightHand",F)
getAngleSimple(leftPants, "leftPants",F)
getAngleSimple(rightPants, "rightPants",F)
getAngleSimple(glass, "glass",F)

