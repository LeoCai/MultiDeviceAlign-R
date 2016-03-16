source("preprocess.R")
source("../handshake_9_11/model/GloableConvert.R")
source("./utils.R")
source("./getMatrixByMag.R")
# source("./readData.R")
library(zoo)
library(rgl)
# globalAccs = getGlobalAccByMag(top)
# plot(globalAccs[,1],type="l")



topcenter = read.csv("datas/2_18/topcenter.csv");topcenter = topcenter[400:1000,]
globalAccs = getGlobalAccByMag(topcenter)

plot(topcenter$LinearAcc0,type="l")
plot(globalAccs[,2],type="l")

plot(globalAccs[,2],type="l")
plot(globalAccs[,3],type="l")

plot(globalAccs[,1],type="l")
plot(globalAccs[,2],type="l")
plot(globalAccs[,3],type="l")

plot(topcenter$LinearAcc1,type="l")

cor(topcenter$LinearAcc1,globalAccs[,3])
cor(topcenter$LinearAcc2,globalAccs[,1])
cor(topcenter$LinearAcc0,globalAccs[,1])


plot(topcenter$ConvertedData2,type="l")
plot(topcenter$LinearAcc1,type="l")

cor(topcenter$LinearAcc1,topcenter$ConvertedData2)
cor(topcenter$LinearAcc0,topcenter$ConvertedData0)
cor(topcenter$LinearAcc2,topcenter$ConvertedData1)


lacc = cbind(topcenter$LinearAcc0,topcenter$LinearAcc1,topcenter$LinearAcc2)

plot(lacc[,3],type = "l",main="forward acc",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)
plot(lacc[,1],type = "l",main="left-right acc",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)
plot(lacc[,2],type = "l",main="gravity acc",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)

print("left-right")
mean(lacc[,1])
sd(lacc[,1])
mean(lacc[lacc[,3]>0,1])
sd(lacc[lacc[,3]>0,1])

print("forward")
mean(lacc[,3])
sd(lacc[,3])
mean(lacc[lacc[,3]>0,3])
sd(lacc[lacc[,3]>0,3])

print("gravity")
mean(lacc[,2])
sd(lacc[,2])
mean(lacc[lacc[,3]>0,2])
sd(lacc[lacc[,3]>0,2])
# matplot(1:nrow(topcenter),cbind(topcenter$LinearAcc0,-topcenter$ConvertedData0),type = "l")
# matplot(1:nrow(topcenter),cbind(topcenter$LinearAcc2,topcenter$ConvertedData1),type = "l")


(pc13D = pcaProcess(lacc)) #PCA3D
plotPC13D = rbind(c(0,0,0),pc13D)
plotPC13D
lim3d = c(-1,1)
plot3d(x = plotPC13D[,1], y = plotPC13D[,3], z=plotPC13D[,2], type="l",xlab = "right",ylab="forward",zlab="up", xlim = lim3d, ylim = lim3d,
       zlim = lim3d,col="red",main="PCA_PC1_3D",cex.main=1.5,cex.lab=1.5,lwd = 1.5)

plot(lacc[,2],type = "l",main="gravity")

lacc[,2] = rep(0,nrow(lacc))


plot(lacc[,3],type = "l",main="forward")
plot(lacc[,1],type = "l",main="left-right")
plot(lacc[,2],type = "l",main="gravity")


# pcaProcess(lacc[lacc[,2]>=0,])
pc12D = pcaProcess(lacc)#PCA2D
plotPC23D = rbind(c(0,0,0),pc12D)
plotPC23D
lim3d = c(-1,1)
plot3d(x = plotPC23D[,1], y = plotPC23D[,3], z=plotPC23D[,2], type="l",xlab = "right",ylab="forward",zlab="up", xlim = lim3d, ylim = lim3d, zlim = lim3d,col="red"
       ,main="PCA_PC1_2D",cex.main=1.5,cex.lab=1.5,lwd = 1.5)

pcaProcess(lacc[lacc[,3]>=0,])

filterLacc = lacc[lacc[,3]>=0,]
matplot(1:nrow(filterLacc),filterLacc,type = "l")



arrows3d = function(v,lim3d = c(-1,1),title="3d"){
  open3d()
  plot3d(x= c(0,0,0),xlim = lim3d, ylim = lim3d, zlim = lim3d,main  =title, cex.main = 1.5, cex.lab = 1.5,xlab = "right",ylab="forward",zlab="up" )
  vec=
  segments3d( vec )
  cone3d(base=vec[2,]-(vec[1,]+vec[2,]/6), 
         #this makes the head go 1/6th the length of the arrow
         rad=0.05,
         tip=vec[2,],
         col="blue",
         front="lines",
         back="lines")
}
# arrows3d(c(2,1,1))



