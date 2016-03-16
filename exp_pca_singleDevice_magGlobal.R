source("preprocess.R")
source("../handshake_9_11/model/GloableConvert.R")
source("./utils.R")

glassRaw = read.csv("./datas/2_17/Glass_3.csv")
topRaw = read.csv("./datas/2_17/Top_3.csv")
leftPantsRaw = read.csv("./datas/2_17/LeftPants_3.csv")
rightPantsRaw = read.csv("./datas/2_17/RightPants_3.csv")
leftHandRaw = read.csv("./datas/2_17/LeftHand_3.csv")
rightHandRaw = read.csv("./datas/2_17/RightHand_3.csv")

#select index

#filter gravity

dataFiltedGlass = filterGravity(glassRaw$Acc0,glassRaw$Acc1,glassRaw$Acc2,nrow(glassRaw))
dataFiltedTop = filterGravity(topRaw$Acc0,topRaw$Acc1,topRaw$Acc2,nrow(topRaw))
dataFiltedLeftPants = filterGravity(leftPantsRaw$Acc0,leftPantsRaw$Acc1,leftPantsRaw$Acc2,nrow(leftPantsRaw))
dataFiltedRightPants = filterGravity(rightPantsRaw$Acc0,rightPantsRaw$Acc1,rightPantsRaw$Acc2,nrow(rightPantsRaw))
dataFiltedLeftHand = filterGravity(leftHandRaw$Acc0,leftHandRaw$Acc1,leftHandRaw$Acc2,nrow(leftHandRaw))
dataFiltedRightHand = filterGravity(rightHandRaw$Acc0,rightHandRaw$Acc1,rightHandRaw$Acc2,nrow(rightHandRaw))

dataFiltedGlass[[1]]
glass = cbind(glassRaw, dataFiltedGlass)
top = cbind(topRaw, dataFiltedTop)
leftPants = cbind(leftPantsRaw, dataFiltedLeftPants)
rightPants = cbind(rightPantsRaw, dataFiltedRightPants)
leftHand = cbind(leftHandRaw, dataFiltedLeftHand)
rightHand = cbind(rightHandRaw, dataFiltedRightHand)

#preprocess: gyro update

updateMatrixByMultiGYR = function(gyrs, initMatrix) {
  cuMatrix = initMatrix
  matrixs = list()
  (lenG = nrow(gyrs))
  for (i in 1:lenG) {
    # cuMatrix = updateMatrixByGYR(gyrs[i,1:3],gyrs[i,4],cuMatrix)
    if(gyrs[i,4]!=0)
      cuMatrix = updateMatrixByGYR(gyrs[i,1:3],gyrs[i,4],cuMatrix)
    matrixs = rbind(matrixs,as.vector(cuMatrix))
  }
  return (matrixs)
}

updateFrameByGryo = function(data) {
  gyrs = cbind(data$Gyro0,data$Gyro1,data$Gyro2,data$dt)
  initMatrix = I
  rtms = updateMatrixByMultiGYR(gyrs, initMatrix)
  accs = cbind(data$lv1,data$lv2,data$lv3)
  globalAccs = getGlobleAccs(accs, rtms)
  k = data.frame()
  for(i in 1:nrow(globalAccs)){
    k = rbind(k,unitVector(as.numeric(globalAccs[i,])))
  }
  return(k)
}

glassRaw

glassGlobalGyro = updateFrameByGryo(glass)
topGlobalGyro = cbind(top$ConvertedData0,top$ConvertedData1,top$ConvertedData2)
leftPantsGlobalGyro = cbind(leftPants$ConvertedData0,leftPants$ConvertedData1,leftPants$ConvertedData2)
rightPantsGlobalGyro = cbind(rightPants$ConvertedData0,rightPants$ConvertedData1,rightPants$ConvertedData2)
leftHandGlobalGyro = cbind(leftHand$ConvertedData0,leftHand$ConvertedData1,leftHand$ConvertedData2)
rightHandGlobalGyro = cbind(rightHand$ConvertedData0,rightHand$ConvertedData1,rightHand$ConvertedData2)

#PCA proces:
pcaProcess = function(data){
  epca6A <- prcomp(data,
                   center = TRUE,
                   scale. = FALSE)
  data = predict(epca6A, newdata = data)
  return(epca6A$rotation[,1])
}
pcaVectorGlass = pcaProcess(glassGlobalGyro)
pcaVectorTop = pcaProcess(topGlobalGyro)
pcaVectorLeftPants = pcaProcess(leftPantsGlobalGyro)
pcaVectorRightPants = pcaProcess(rightPantsGlobalGyro)
pcaVectorLeftHand = pcaProcess(leftHandGlobalGyro)
pcaVectorRightHand = pcaProcess(rightHandGlobalGyro)

computeMagVector = function(gx,mg){
  Ax = gx[,1]; Ay = gx[,2]; Az = gx[,3];
  Ex = mg[,1]; Ey = mg[,2]; Ez = mg[,3];
  Hx = Ey*Az - Ez*Ay;
  Hy = Ez*Ax - Ex*Az;
  Hz = Ex*Ay - Ey*Ax;
  normH = sqrt(Hx*Hx + Hy*Hy + Hz*Hz);
  invH = 1.0 / normH;
  Hx = Hx*invH;
  Hy = Hy*invH;
  Hz = Hz*invH;
  invA = 1.0 / sqrt(Ax*Ax + Ay*Ay + Az*Az);
  Ax = Ax*invA;
  Ay = Ay*invA;
  Az = Az*invA;
  Mx = Ay*Hz - Az*Hy;
  My = Az*Hx - Ax*Hz;
  Mz = Ax*Hy - Ay*Hx;
  return (c(mean(Mx),mean(My),mean(Mz)))
}
head(glass)
head(top)
head(glass[,12:14])
head(glass[,7:9])
#GM compare:
magVectorGlass = computeMagVector(glass[,12:14],glass[,7:9])
magVectorTop = computeMagVector(top[,22:24],top[,13:15])
magVectorLeftPants = computeMagVector(leftPants[,22:24],leftPants[,7:9])
magVectorRightPants = computeMagVector(rightPants[,22:24],rightPants[,7:9])
magVectorLeftHand = computeMagVector(leftHand[,22:24],leftHand[,7:9])
magVectorRightHand = computeMagVector(rightHand[,22:24],rightHand[,7:9])




#draw 3d:

#project on horizental

# l = data.frame(); g= data.frame()
# l = rbind(c(1,0,0)); g= rbind(c(0,1,0))
# computeProjOnGravaty(l,g)[1,]
# computeProjOnHori(l,g)[1,]
# sum = 0
# for(i in 1:3){
#   sum = sum+computeProjOnGravaty(l,g)[1,i]*computeProjOnHori(l,g)[1,i]
# }
# 
# sum

horiGlass = computeProjOnHori(glassGlobalGyro, cbind(glass$gv1[1],glass$gv2[1], glass$gv3[1]))
horiTop = computeProjOnHori(topGlobalGyro, cbind(top$gv1[1],top$gv2[1], top$gv3[1]))
horiLeftPants =  computeProjOnHori(leftPantsGlobalGyro, cbind(leftPants$gv1[1],leftPants$gv2[1], leftPants$gv3[1]))
horiRightPants = computeProjOnHori(rightPantsGlobalGyro, cbind(rightPants$gv1[1],rightPants$gv2[1], rightPants$gv3[1]))
horiLeftHand = computeProjOnHori(leftHandGlobalGyro, cbind(leftHand$gv1[1],leftHand$gv2[1], leftHand$gv3[1]))
horiRightHand = computeProjOnHori(rightHandGlobalGyro, cbind(rightHand$gv1[1],rightHand$gv2[1], rightHand$gv3[1]))

pcaHoriGlass = pcaProcess(horiGlass)
pcaHoriTop = pcaProcess(horiTop)
pcaHoriLeftPants = pcaProcess(horiLeftPants)
pcaHoriRightPants = pcaProcess(horiRightPants)
pcaHoriLeftHand = pcaProcess(horiLeftHand)
pcaHoriRightHand = pcaProcess(horiRightHand)

# computeAngle(pcaVectorGlass,magVectorGlass)
computeAngle(pcaVectorTop,magVectorTop)
computeAngle(pcaVectorLeftPants,magVectorLeftPants)
computeAngle(pcaVectorRightPants,magVectorRightPants)
computeAngle(pcaVectorLeftHand,magVectorLeftHand)
computeAngle(pcaVectorRightHand,magVectorRightHand)

# computeAngle(pcaHoriGlass,magVectorGlass)
computeAngle(pcaHoriTop,magVectorTop)
computeAngle(pcaHoriLeftPants,magVectorLeftPants)
computeAngle(pcaHoriRightPants,magVectorRightPants)
computeAngle(pcaHoriLeftHand,magVectorLeftHand)
computeAngle(pcaHoriRightHand,magVectorRightHand)

