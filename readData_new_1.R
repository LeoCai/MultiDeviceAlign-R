topRaw = read.csv("./datas/3_1_data/top/11.csv");topRaw = sampleByData(topRaw); topRaw = topRaw[100:900,]
leftHandRaw = read.csv("./datas/3_1_data/lefthand/11.csv");leftHandRaw = sampleByData(leftHandRaw);  leftHandRaw = leftHandRaw[110:910,]
rightHandRaw = read.csv("./datas/3_1_data/righthand/11.csv");rightHandRaw = sampleByData(rightHandRaw);  rightHandRaw = rightHandRaw[96:896,]
leftPantsRaw = read.csv("./datas/3_1_data/leftpants/11.csv");leftPantsRaw = sampleByData(leftPantsRaw);  leftPantsRaw = leftPantsRaw[95:895,]
rightPantsRaw = read.csv("./datas/3_1_data/rightpants/11.csv");rightPantsRaw = sampleByData(rightPantsRaw);  rightPantsRaw = rightPantsRaw[95:895,]
glassRaw = read.csv("./datas/3_1_data/glass/11.csv");glassRaw = sampleByData(glassRaw);  glassRaw = glassRaw[104:904,]


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
