source("preprocess.R")
source("../handshake_9_11/model/GloableConvert.R")
source("./utils.R")
source("./smartfilterFunction.R")
library(zoo)
library(rgl)


topcenter = read.csv("datas/2_18/topcenter.csv");topcenter = topcenter[450:800,]

lacc = cbind(topcenter$LinearAcc0,topcenter$LinearAcc1,topcenter$LinearAcc2)

plot(topcenter$LinearAcc2,type = "l")

plot(topcenter$ConvertedData1,type = "l")
cor(topcenter$LinearAcc2, topcenter$ConvertedData1)
cor(topcenter$LinearAcc0, topcenter$ConvertedData0)


plot(lacc[,3],type = "l",main="forward acc",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)
plot(lacc[,1],type = "l",main="left-right acc",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)
plot(lacc[,2],type = "l",main="gravity acc",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)
print("forward statics")
mean(lacc[lacc[,3]>0,3])
sd(lacc[lacc[,3]>0,3])

print("left_right statics")
mean(lacc[lacc[,1]>0,1])
sd(lacc[lacc[,1]>0,1])

print("gravity statics")
mean(lacc[lacc[,3]>0,3])
sd(lacc[lacc[,3]>0,3])




filted_forward = smartFilter(lacc[,3],maxThrehold = 1,tag = "Forward")
# points(x= c(1,100),y = c(1, -1))
filted_left_right = smartFilter(lacc[,1],maxThrehold = 1,tag = "Left Right")

print("filted_forward")
(mean_forward = mean(filted_forward[filted_forward!=0]))
sd(filted_forward[filted_forward!=0])

print("filted_left_right")
(mean_left_right = mean(filted_left_right[filted_left_right!=0]))
sd(filted_left_right[filted_left_right!=0])

arrows3d(c(mean_left_right,mean_forward,0),lim3d = c(-0.3,0.3),title = "3d vector")

# # filterLacc_forward = rollapply(lowpassLacc_forward,1, function(x){
# #   if(x[1])
# # })
# # filterAcc_forward = c()
# # for(i in 1:nrow(filterLacc_forward)){
# #   filterAcc_forward = c(filterAcc_forward, filterLacc_forward[i,])
# # }
# 
# plot(filterLacc_forward,type = "l",main="forward acc",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)
# 
# 
# 
# print("left-right")
# mean(lacc[,1])
# sd(lacc[,1])
# mean(lacc[lacc[,3]>0,1])
# sd(lacc[lacc[,3]>0,1])
# 
# print("forward")
# mean(lacc[,3])
# sd(lacc[,3])
# mean(lacc[lacc[,3]>0,3])
# sd(lacc[lacc[,3]>0,3])
# 
# print("gravity")
# mean(lacc[,2])
# sd(lacc[,2])
# mean(lacc[lacc[,3]>0,2])
# sd(lacc[lacc[,3]>0,2])
# 
# lacc[lacc[,3]< -0.1,3]  = 0
# plot(lacc[,3],type = "l",main="forward acc filterd",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5,ylim = c(-1.5,1.5))
# 
# lacc[lacc[,1]< -0.1,3]  = 0
# plot(lacc[,3],type = "l",main="forward acc filterd",ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5,ylim = c(-1.5,1.5))
# # matplot(1:nrow(topcenter),cbind(topcenter$LinearAcc0,-topcenter$ConvertedData0),type = "l")
# # matplot(1:nrow(topcenter),cbind(topcenter$LinearAcc2,topcenter$ConvertedData1),type = "l")
# 
# 
# (pc13D = pcaProcess(lacc)) #PCA3D
# plotPC13D = rbind(c(0,0,0),pc13D)
# plotPC13D
# lim3d = c(-1,1)
# plot3d(x = plotPC13D[,1], y = plotPC13D[,3], z=plotPC13D[,2], type="l",xlab = "right",ylab="forward",zlab="up", xlim = lim3d, ylim = lim3d,
#        zlim = lim3d,col="red",main="PCA_PC1_3D",cex.main=1.5,cex.lab=1.5,lwd = 1.5)
# 
# plot(lacc[,2],type = "l",main="gravity")
# 
# lacc[,2] = rep(0,nrow(lacc))
# 
# 
# plot(lacc[,3],type = "l",main="forward")
# plot(lacc[,1],type = "l",main="left-right")
# plot(lacc[,2],type = "l",main="gravity")
# 
# 
# # pcaProcess(lacc[lacc[,2]>=0,])
# pc12D = pcaProcess(lacc)#PCA2D
# plotPC23D = rbind(c(0,0,0),pc12D)
# plotPC23D
# lim3d = c(-1,1)
# plot3d(x = plotPC23D[,1], y = plotPC23D[,3], z=plotPC23D[,2], type="l",xlab = "right",ylab="forward",zlab="up", xlim = lim3d, ylim = lim3d, zlim = lim3d,col="red"
#        ,main="PCA_PC1_2D",cex.main=1.5,cex.lab=1.5,lwd = 1.5)
# 
# pcaProcess(lacc[lacc[,3]>=0,])
# 
# filterLacc = lacc[lacc[,3]>=0,]
# matplot(1:nrow(filterLacc),filterLacc,type = "l")
# 
# 
# 
# arrows3d(c(2,1,1))
# 
# 
# 
