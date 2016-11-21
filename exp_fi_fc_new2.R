#Author: LeoCai
#根据相关性和平均值分别求角度

source("./utils.R")
source("./time_align_3_9.R")
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

magAngleSmall = c(-5.2535963089,-85.6291941575,-9.6232952834)
magAngleLarge = c(-5.2535963089,-5.2535963089,-5.2535963089)
window = 5
angleResults = matrix(nrow=24,ncol = 6)
pcaCorResults = matrix(nrow=24,ncol = 6)
fileParent = "3_10_data"
smallOrLarge = "large"

getAngleSimple = function(mdata,magDrift, tag = "", plot3d = F) {
  globalAcc = getGlobalAccByMag(mdata)[selectIndex,]
  globalAcc[,3] = 0
  # matplot(cbind(Fc[1:150],globalAcc[1:150,2]),type = "l",main = paste(tag, "fc_fi"))
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
  angle = computeAngle(results,c(0,1,0))
  sig = getDirection(results, c(0,1,0),c(0,0,1))
  angle = sig*angle
  angle = angle - magDrift
  print(tag)
  # print(results)
  # print(angle)
  if (plot3d) {
    init3d(tag)
    myArrow3d(results * 4,2)
    for (i in 1:nrow(globalAcc)) {
      myArrow3d(globalAcc[i,])
    }
  }
  
  radAngle = angle/180*pi
  rtm = matrix(c(cos(radAngle), sin(radAngle), 0, -sin(radAngle), cos(radAngle),0, 0, 0, 1),nrow = 3,ncol = 3)
  pcaGlobal = c()
  globalAcc =  getGlobalAccByMag(mdata)[selectIndex,]
  for(i in 1:nrow(globalAcc)){
    pcaGlobal = rbind(pcaGlobal, rotationByMatrix(globalAcc[i,],rtm))
  }
  meanCor = 0
  for(i in 1:2){
    meanCor = meanCor + cor(pcaGlobal[,i],globalAcc[,i])
  }
  meanCor = meanCor/2
  print(paste(tag,"pca_mag_cor:",meanCor))
  
  return(c(angle,meanCor))
}

addResults = function(results,path,i,deviceNum){
  angleResults[(path-1)*8+i,deviceNum] <<- results[1]; 
  pcaCorResults[(path-1)*8+i,deviceNum] <<- results[2]; 
  deviceNum<<- deviceNum+1
}

for(smallOrLarge in c("small")){
  for(window in c(5)){
    selectIndex <<- 1:(50*window)
    
    if(smallOrLarge == "large") {
      pathIndex <<- 4:6
    }else{
      pathIndex <<- 1:3
    }
    
    for(path in pathIndex){
      for(i in 1:5){
        
        print(paste(smallOrLarge,"window:",window,"path",path,"i",i,"--------------------------------"))
        
        readSample(path, i, s, e)
        
        addDeviceDataToPCA = addDeviceDataToPCA2
        
        gacctop = getGlobalAccByMag(top)
        
        Fc <<- computeFc()
        cor_Fc_Linear = cor(Fc, top$LinearAcc2[selectIndex])
        
        print(paste("cor_Fc_Linear",cor_Fc_Linear))
        
        deviceNum = 1
        
        if(smallOrLarge == "large") {
          rowIndex = path-3 
          magDrift = magAngleLarge[rowIndex]
        }
        else {
          rowIndex = path
          magDrift = magAngleSmall[rowIndex]   
        }
        results = getAngleSimple(glass,magDrift, "glass",F); addResults(results,rowIndex,i,deviceNum); 
        results = getAngleSimple(top,magDrift, "top",F); addResults(results,rowIndex,i,deviceNum);
        results = getAngleSimple(leftHand,magDrift, "leftHand",F); addResults(results,rowIndex,i,deviceNum); 
        results = getAngleSimple(rightHand, magDrift,"rightHand",F); addResults(results,rowIndex,i,deviceNum); 
        results = getAngleSimple(leftPants, magDrift,"leftPants",F); addResults(results,rowIndex,i,deviceNum); 
        results = getAngleSimple(rightPants,magDrift, "rightPants",F);addResults(results,rowIndex,i,deviceNum); 
      }
    }
    angleResults = as.data.frame(angleResults);names(angleResults) = c("glass","top","lefthand","righthand","leftpants","rightpants")
    pcaCorResults = as.data.frame(pcaCorResults);names(pcaCorResults) = c("glass","top","lefthand","righthand","leftpants","rightpants")
    write.csv(angleResults,paste("./summary_fi_fc_pca_angle_cor/angleResults_",smallOrLarge,"_",window,".csv",sep = ""),row.names = F)
    write.csv(pcaCorResults,paste("./summary_fi_fc_pca_angle_cor/pcaCorResults_",smallOrLarge,"_",window,".csv",sep = ""),row.names = F)
    print(paste("angle results",angleResults))
    # print(pcaCorResults)
  }
}



