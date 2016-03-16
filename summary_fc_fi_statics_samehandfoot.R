source("./utils.R")
source("./timealign_3_3_speedud.R")
source("./getMatrixByMag.R")
source("./addPCAData.R")
source("./plot_debug.R")
library(zoo)

selectIndex = 1:300

addDeviceDataToPCA = addDeviceDataToPCA2

gacctop = getGlobalAccByMag(top)

# Fc Fcv Fi Fin

Fc = computeFc()

fcbar = c()
fibar = c()
finbar = c()

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
    Fcv = rbind(Fcv,Fc[i] * results)
  }
  Fin = Fcv - Fi
  meanFcv = c(mean(Fcv[,1]), mean(Fcv[,2]), mean(Fcv[,3]))
  sdFcv = c(sd(Fcv[,1]), sd(Fcv[,2]), sd(Fcv[,3]))
  
  meanFi = c(mean(Fi[,1]), mean(Fi[,2]), mean(Fi[,3]))
  sdFi = c(sd(Fi[,1]), sd(Fi[,2]), sd(Fi[,3]))
  
  meanFin =  c(mean(Fin[,1]), mean(Fin[,2]), mean(Fin[,3]))
  sdFin =  c(sd(Fin[,1]), sd(Fin[,2]), sd(Fin[,3]))
  fcbar <<-rbind(fcbar, c(meanFcv[2],meanFcv[1],sdFcv[2],sdFcv[1]))
  fibar <<-rbind(fibar, c(meanFi[2],meanFi[1],sdFi[2],sdFi[1]))
  finbar <<-rbind(finbar, c(meanFin[2],meanFin[1],sdFin[2],sdFin[1]))
  
  
  # write.csv(cbind(meanFcv,meanFi,meanFin,sdFcv,sdFi,sdFin),paste("./summary_fc_fi/",tag,"_statics_normal.csv",sep=""),row.names = F)
  
  
  FcMagnitude  = sqrt(Fcv[,1] ^ 2 + Fcv[,2] ^ 2 + Fcv[,3] ^ 2)
  FinMagnitude = sqrt(Fin[,1] ^ 2 + Fin[,2] ^ 2 + Fin[,3] ^ 2)
  
  # Fin_v_mag = cbind(Fin,FinMagnitude)
  #   write.csv(Fin_v_mag,paste("./summary_fc_fi/",tag,"_fin_vector_magnitude.csv"),row.names = F)
  
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

fcbar = as.data.frame(fcbar) 
names(fcbar) = c("mean_forward","mean_right","sd_forward","sd_right")
fibar = as.data.frame(fibar)
names(fibar) = c("mean_forward","mean_right","sd_forward","sd_right")
finbar = as.data.frame(finbar)
names(finbar) = c("mean_forward","mean_right","sd_forward","sd_right")
write.csv(fcbar,"./summary_fc_fi/speedup_fcbar.csv",row.names = F)
write.csv(fibar,"./summary_fc_fi/speedup_fibar.csv",row.names = F)
write.csv(finbar,"./summary_fc_fi/speedup_finbar.csv",row.names = F)