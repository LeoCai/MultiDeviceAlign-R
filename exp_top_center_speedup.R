source("preprocess.R")
source("../handshake_9_11/model/GloableConvert.R")
source("./utils.R")
library(zoo)
library(rgl)


topcenter_speedup = read.csv("datas/2_19/top_center_speedup.csv");

lacc = cbind(topcenter_speedup$LinearAcc0,topcenter_speedup$LinearAcc1,topcenter_speedup$LinearAcc2)

plot(lacc[,3],type = "l",main="forward acc",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)
plot(lacc[,1],type = "l",main="left-right acc",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)
plot(lacc[,2],type = "l",main="gravity acc",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)

print("left-right")
mean(lacc[,1])
sd(lacc[,1])
mean(lacc[lacc[,1]>0,1])
sd(lacc[lacc[,1]>0,1])

print("forward")
mean(lacc[,3])
sd(lacc[,3])
mean(lacc[lacc[,3]>0,3])
sd(lacc[lacc[,3]>0,3])

print("gravity")
mean(lacc[,2])
sd(lacc[,2])
mean(lacc[lacc[,2]>0,2])
sd(lacc[lacc[,2]>0,2])
# matplot(1:nrow(topcenter),cbind(topcenter$LinearAcc0,-topcenter$ConvertedData0),type = "l")
# matplot(1:nrow(topcenter),cbind(topcenter$LinearAcc2,topcenter$ConvertedData1),type = "l")

lacc2 = lacc[(300:800)+450,]
lacc2 = lacc2[60:460,]
mean(lacc2[0:200,3])
mean(lacc2[200:400,3])
mean(lacc2[0:200,1])
mean(lacc2[0:200,2])

plot(lacc2[,3],type = "l",main="forward acc single period",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)
plot(lacc2[,1],type = "l",main="left_right acc single period",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)


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
  vec=rbind( c( 0, 0, 0 ), v )
  segments3d( vec )
  cone3d(base=vec[2,]-(vec[1,]+vec[2,]/6), 
         #this makes the head go 1/6th the length of the arrow
         rad=0.1,
         tip=vec[2,],
         col="blue",
         front="lines",
         back="lines")
}
arrows3d(c(2,1,1))



