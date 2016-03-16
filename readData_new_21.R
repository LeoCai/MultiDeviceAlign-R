topRaw = read.csv("./datas/3_1_data/top/21.csv");topRaw = sampleByData(topRaw); topRaw = topRaw[100:900,]
leftHandRaw = read.csv("./datas/3_1_data/lefthand/21.csv");leftHandRaw = sampleByData(leftHandRaw);  leftHandRaw = leftHandRaw[105:905,]
rightHandRaw = read.csv("./datas/3_1_data/righthand/21.csv");rightHandRaw = sampleByData(rightHandRaw);  rightHandRaw = rightHandRaw[99:899,]
leftPantsRaw = read.csv("./datas/3_1_data/leftpants/21.csv");leftPantsRaw = sampleByData(leftPantsRaw);  leftPantsRaw = leftPantsRaw[110:910,]
rightPantsRaw = read.csv("./datas/3_1_data/rightpants/21.csv");rightPantsRaw = sampleByData(rightPantsRaw);  rightPantsRaw = rightPantsRaw[110:910,]
glassRaw = read.csv("./datas/3_1_data/glass/21.csv");glassRaw = sampleByData(glassRaw);  glassRaw = glassRaw[99:899,]


dataFiltedGlass = filterGravity(glassRaw$Acc0,glassRaw$Acc1,glassRaw$Acc2,nrow(glassRaw))
dataFiltedTop = filterGravity(topRaw$Acc0,topRaw$Acc1,topRaw$Acc2,nrow(topRaw))
dataFiltedLeftPants = filterGravity(leftPantsRaw$Acc0,leftPantsRaw$Acc1,leftPantsRaw$Acc2,nrow(leftPantsRaw))
dataFiltedRightPants = filterGravity(rightPantsRaw$Acc0,rightPantsRaw$Acc1,rightPantsRaw$Acc2,nrow(rightPantsRaw))
dataFiltedLeftHand = filterGravity(leftHandRaw$Acc0,leftHandRaw$Acc1,leftHandRaw$Acc2,nrow(leftHandRaw))
dataFiltedRightHand = filterGravity(rightHandRaw$Acc0,rightHandRaw$Acc1,rightHandRaw$Acc2,nrow(rightHandRaw))

glass = cbind(glassRaw, dataFiltedGlass)
top = cbind(topRaw, dataFiltedTop)
leftPants = cbind(leftPantsRaw, dataFiltedLeftPants)
rightPants = cbind(rightPantsRaw, dataFiltedRightPants)
leftHand = cbind(leftHandRaw, dataFiltedLeftHand)
rightHand = cbind(rightHandRaw, dataFiltedRightHand)


mylowpass = function(top){
  return(as.data.frame(rollapply(top,3,mean)))
}
top = mylowpass(top)
leftHand = mylowpass(leftHand)
rightHand = mylowpass(rightHand)
leftPants = mylowpass(leftPants)
rightPants = mylowpass(rightPants)
