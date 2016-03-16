source("./utils.R")
source("./timealign_3_2.R")
source("./getMatrixByMag.R")
source("./addPCAData.R")
source("./plot_debug.R")
library(zoo)

selectIndex = 1:699

addDeviceDataToPCA = addDeviceDataToPCA2

gacctop = getGlobalAccByMag(top)

# Fc Fcv Fi Fin

Fc = computeFc()

getAngleSimple = function(mdata, tag = "", plot3d = F) {
  globalAcc = getGlobalAccByMag(mdata)[selectIndex,]
  globalAcc[,3] = 0
  #   a = 23/180*pi
  #   rm = matrix(c(cos(a),sin(a),0,sin(a),cos(a),0,0,0,1),nrow = 3,ncol = 3, byrow = T)
  #   for(i in 1:nrow(globalAcc)){
  #     globalAcc[i,] = rotationByMatrix(globalAcc[i,],rm)
  #   }
  Fi = globalAcc
  tr = 0.01; results = c()
  dimStr = c("x","y","z")
  for (dim in 1:3) {
    # mean = 0;count = 0
    fiFc = c()
    EFi = mean(Fi[,dim])
    EFc = mean(Fc)
    print(paste(EFi,EFc))
    meanR = EFi / EFc
    results = c(results,meanR)
  }
  
  Fcv = c()
  for (i in 1:length(Fc)) {
    Fcv = rbind(Fcv,Fc[i] * unitVector(results))
  }
  Fin = Fcv - Fi
  Fin = as.data.frame(Fin)
  names(Fin) = c("Finx","Finy","Finz")
  
  FcMagnitude  = sqrt(Fcv[,1] ^ 2 + Fcv[,2] ^ 2 + Fcv[,3] ^ 2)
  FinMagnitude = sqrt(Fin[,1] ^ 2 + Fin[,2] ^ 2 + Fin[,3] ^ 2)
  
  Fin_v_mag = cbind(Fin,FinMagnitude,FcMagnitude)
  write.csv(as.data.frame(results),paste("./summary_fc_fi/",tag,"_fc_direction.csv",sep = ""),row.names = F)
  write.csv(Fin_v_mag,paste("./summary_fc_fi/",tag,"_fin_vector_magnitude.csv",sep = ""),row.names = F)
  
  angle = computeAngle(results,c(0,1,0))
  sig = getDirection(results, c(0,1,0),c(0,0,1))
  print(tag)
  print(results)
  print(sig * angle)
  if (plot3d) {
    init3d(tag)
    myArrow3d(results * 4,2)
    for (i in 1:nrow(globalAcc)) {
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
