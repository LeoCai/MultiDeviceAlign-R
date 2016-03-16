source("preprocess.R")
source("../handshake_9_11/model/GloableConvert.R")
source("./utils.R")

glassRaw = read.csv("./datas/2_17/Glass_3.csv")
topRaw = read.csv("./datas/2_17/Top_3.csv")
leftPantsRaw = read.csv("./datas/2_17/LeftPants_3.csv")
rightPantsRaw = read.csv("./datas/2_17/RightPants_3.csv")
leftHandRaw = read.csv("./datas/2_17/LeftHand_3.csv")
rightHandRaw = read.csv("./datas/2_17/RightHand_3.csv")


mean(topRaw$ConvertedData0)
mean(topRaw$ConvertedData1)
mean(topRaw$ConvertedData2)

sd(rightHandRaw$ConvertedData0)
sd(rightHandRaw$ConvertedData1)
sd(rightHandRaw$ConvertedData2)
