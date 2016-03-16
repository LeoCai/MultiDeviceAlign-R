glassRaw = read.csv("./datas/2_17/Glass_3.csv"); glassRaw = glassRaw[20:200,]
topRaw = read.csv("./datas/2_17/Top_3.csv"); topRaw = topRaw[97:797,]
leftPantsRaw = read.csv("./datas/2_17/LeftPants_3.csv"); leftPantsRaw = leftPantsRaw[100:800,]
rightPantsRaw = read.csv("./datas/2_17/RightPants_3.csv"); rightPantsRaw = rightPantsRaw[100:800,]
leftHandRaw = read.csv("./datas/2_17/LeftHand_3.csv"); leftHandRaw = leftHandRaw[111:811,]
rightHandRaw = read.csv("./datas/2_17/RightHand_3.csv"); rightHandRaw = rightHandRaw[100:800,]

# dataFiltedGlass = filterGravity(glassRaw$Acc0,glassRaw$Acc1,glassRaw$Acc2,nrow(glassRaw))
# dataFiltedTop = filterGravity(topRaw$Acc0,topRaw$Acc1,topRaw$Acc2,nrow(topRaw))
# dataFiltedLeftPants = filterGravity(leftPantsRaw$Acc0,leftPantsRaw$Acc1,leftPantsRaw$Acc2,nrow(leftPantsRaw))
# dataFiltedRightPants = filterGravity(rightPantsRaw$Acc0,rightPantsRaw$Acc1,rightPantsRaw$Acc2,nrow(rightPantsRaw))
# dataFiltedLeftHand = filterGravity(leftHandRaw$Acc0,leftHandRaw$Acc1,leftHandRaw$Acc2,nrow(leftHandRaw))
# dataFiltedRightHand = filterGravity(rightHandRaw$Acc0,rightHandRaw$Acc1,rightHandRaw$Acc2,nrow(rightHandRaw))
# 
# glass = cbind(glassRaw, dataFiltedGlass)
# top = cbind(topRaw, dataFiltedTop)
# leftPants = cbind(leftPantsRaw, dataFiltedLeftPants)
# rightPants = cbind(rightPantsRaw, dataFiltedRightPants)
# leftHand = cbind(leftHandRaw, dataFiltedLeftHand)
# rightHand = cbind(rightHandRaw, dataFiltedRightHand)

glass = glassRaw
top = topRaw
leftPants = leftPantsRaw
rightPants = rightPantsRaw
leftHand = leftHandRaw
rightHand = rightHandRaw