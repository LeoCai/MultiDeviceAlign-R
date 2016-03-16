glassRaw = read.csv("./datas/2_17/Glass_1.csv"); glassRaw = glassRaw[25:205,]
topRaw = read.csv("./datas/2_17/Top_1.csv"); topRaw = topRaw[94:794,]
leftPantsRaw = read.csv("./datas/2_17/LeftPants_1.csv"); leftPantsRaw = leftPantsRaw[100:800,]
rightPantsRaw = read.csv("./datas/2_17/RightPants_1.csv"); rightPantsRaw = rightPantsRaw[94:794,]
leftHandRaw = read.csv("./datas/2_17/LeftHand_1.csv"); leftHandRaw = leftHandRaw[108:808,]
rightHandRaw = read.csv("./datas/2_17/RightHand_1.csv"); rightHandRaw = rightHandRaw[100:800,]

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