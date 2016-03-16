source("preprocess.R")
source("../handshake_9_11/model/GloableConvert.R")
source("./utils.R")
source("./readData.R")


lvTop = cbind(top$ConvertedData0,top$ConvertedData1,top$ConvertedData2)
gvTop = c(top$gv1[1],top$gv2[1],top$gv3[1])
projTop = computeProjOnHori(lvTop,gvTop)
absHoriTop = absVector(projTop[,1],projTop[,2],projTop[,3])


mean(absHoriTop[projTop[,1]<0])
# mean(proj[proj[,1]>0,1])

lvLeftHand = cbind(leftHand$ConvertedData0,leftHand$ConvertedData1,leftHand$ConvertedData2)
gvLeftHand = c(leftHand$gv1[1],leftHand$gv2[1],leftHand$gv3[1])
projLeftHand = computeProjOnHori(lvLeftHand,gvLeftHand)
absHoriLeftHand = absVector(projLeftHand[,1],projLeftHand[,2],projLeftHand[,3])
mean(absHoriLeftHand[projLeftHand[,1]<0])

nrow(projTop) 
nrow(projLeftHand)

projLeftHand_Top = projLeftHand - projTop
matplot(x= 1:nrow(top), cbind(top$ConvertedData0,top$ConvertedData1,top$ConvertedData2), type = "l")
matplot(x= 1:nrow(leftHand), cbind(leftHand$ConvertedData0,leftHand$ConvertedData1,leftHand$ConvertedData2), type = "l")
matplot(x= 1:nrow(rightHand), cbind(rightHand$ConvertedData0,rightHand$ConvertedData1,rightHand$ConvertedData2), type = "l")


mean(absVector(projLeftHand[,1],projLeftHand[,2],projLeftHand[,3]))
mean(absVector(projTop[,1],projTop[,2],projTop[,3]))
mean(absVector(projLeftHand_Top[,1],projLeftHand_Top[,2],projLeftHand_Top[,3]))

plot(top$ResultantAcc,type="l")

plot(rollapply(top$ResultantAcc,8,mean),type="l")

projLeftHand[,1]<0

nrow(projLeftHand)
nrow(projTop)

plot(absHoriTop,type="l")
lines(absHoriLeftHand,col= 2, lty =2)

plot(proj[,1],type = "l")
lines(proj[,2],col = "red",lty = 2)
lines(proj[,3],col = "green",lty = 3)
