source("./utils.R")
source("./readData.R")
source("./getMatrixByMag.R")
source("./addPCAData.R")
library(zoo)

selectIndex = 200:500

computeFin = function(myData,Fc, tag) {
  myData = myData[selectIndex,]
  firstData = myData[1,]
  Fi = getGlobalAccByMag(myData)
  Fi[,2] = Fi[,2] - Fc
  matplot(
    Fi, type = "l", main = paste(tag,"Fin"), cex.main = 1.5, cex.lab = 1.5
  )
  legend("topright",c("x", "y", "z"),lty = 1:3,col = 1:3)
  return(Fi)
}

topForward  = getGlobalAccByMag(top)
topForward = topForward[selectIndex,2]
Fc = computeFc()
plot(
  Fc, type = "l", main = "Fc by PCA",cex.main = 1.5, cex.lab = 1.5
)
computeFin(top,Fc,"top")
computeFin(leftHand,Fc,"leftHand")
computeFin(rightHand,Fc,"rightHand")
computeFin(leftPants,Fc,"leftPants")
computeFin(rightPants,Fc,"rightPants")
