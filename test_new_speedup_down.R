top = read.csv("./datas/3_3_data_speedup_down/lefthand/13.csv")
globalAcc = getGlobalAccByMag(top)
plot(globalAcc[,2],type = "l")

mdata = top;tag = "top"
s1 = 100;e1 = 700
s2 = nrow(mdata) - 200; e2 = nrow(mdata)
dataUp = mdata[s1:e1,]
dataDown = mdata[s2:e2,]
gup = getGlobalAccByMag(dataUp)
gdown = getGlobalAccByMag(dataDown)
plot(gup[,2],type = "l",main = paste(tag,"up"))
plot(gdown[,2],type = "l",main = paste(tag,"down"))
