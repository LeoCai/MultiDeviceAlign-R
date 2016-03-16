source("./utils.R")
source("./readData.R")
source("./getMatrixByMag.R")
source("./addPCAData.R")
source("./plot_debug.R")
library(zoo)


mdata = top
horis = c()
for(i in 1:nrow(mdata)){
  lv = cbind(mdata[i,]$LinearAcc0,mdata[i,]$LinearAcc1,mdata[i,]$LinearAcc2)
  gv = cbind(mdata[i,]$Gravity0,mdata[i,]$Gravity1,mdata[i,]$Gravity2)
  hori = computeProjOnHoriSingleData(lv,gv)
  horis = c(horis, absVector1(hori))
}

myData = top
global = getGlobalAccByMag(myData)
global[,3] = 0
firstData = myData[1,]
gvInit = cbind(firstData$Gravity0,firstData$Gravity1,firstData$Gravity2)
mgInit = cbind(firstData$MagnetData0,firstData$MagnetData1,firstData$MagnetData2)

initRotationMatirx = getMatrixByMag(gvInit,mgInit)
horis2 = c()

cbind(firstData$LinearAcc0,firstData$LinearAcc1,firstData$LinearAcc2)
rotationByMatrix(getGlobalAccByMag(myData)[1,],initRotationMatirx)
for(i in 1:nrow(mdata)){
  horis2 = c(horis2, absVector1(rotationByMatrix(global[i,],initRotationMatirx)))
}

cor(horis, horis2)


