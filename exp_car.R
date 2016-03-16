source("preprocess.R")
source("../handshake_9_11/model/GloableConvert.R")
source("./utils.R")
source("./smartfilterFunction.R")
library(zoo)
library(rgl)


car = read.csv("./datas/2_20_car/car2.csv"); car = car[100:5000,]
plot(car$LinearAcc2,type = "l",ylab = "acc m/s^2", main="car speedup-brake forward ", cex.main =1.5,cex.lab = 1.5)
plot(car$LinearAcc0,type = "l",ylab = "acc m/s^2", main="car speedup-brake left right", cex.main =1.5,cex.lab = 1.5)
plot(car$LinearAcc1,type = "l",ylab = "acc m/s^2", main="car speedup-brake gravity ", cex.main =1.5,cex.lab = 1.5)

singlePeriod = car[1250:1400,]
plot(singlePeriod$LinearAcc2,type = "l",ylab = "acc m/s^2", main="car speedup-brake forward single period", cex.main =1.5,cex.lab = 1.5)
plot(singlePeriod$LinearAcc0,type = "l",ylab = "acc m/s^2", main="car speedup-brake left right single period", cex.main =1.5,cex.lab = 1.5)
plot(singlePeriod$LinearAcc1,type = "l",ylab = "acc m/s^2", main="car speedup-brake gravity single period", cex.main =1.5,cex.lab = 1.5)

lacc = as.data.frame(cbind(car$LinearAcc0,car$LinearAcc1,car$LinearAcc2))
names(lacc) = c("LEFT","GRIVIRY","FORWARD")
summary(lacc)

getDataStatic = function(singlePeriod,tag){
  lacc = as.data.frame(cbind(singlePeriod$LinearAcc0,singlePeriod$LinearAcc1,singlePeriod$LinearAcc2))
  names(lacc) = c("LEFT","GRIVIRY","FORWARD")
  # summary(lacc)
  # print("forward")
  data = lacc$FORWARD
  static_forward = cbind("FORWARD",max(data),min(data),mean(data),sd(data),mean(lacc[lacc$FORWARD>0,]$FORWARD),sd(lacc[lacc$FORWARD>0,]$FORWARD))
  
  data = lacc$LEFT
  static_left = cbind("LEFT",max(data),min(data),mean(data),sd(data),mean(lacc[lacc$LEFT>0,]$LEFT),sd(lacc[lacc$LEFT>0,]$LEFT))
  
  data = lacc$GRIVIRY
  static_grivity = cbind("GRAVITY",max(data),min(data),mean(data),sd(data),mean(lacc[lacc$GRIVIRY>0,]$GRIVIRY),sd(lacc[lacc$GRIVIRY>0,]$GRIVIRY))
  statics = data.frame()
  statics = rbind(as.vector(static_forward),as.vector(static_left),as.vector(static_grivity))
  statics = as.data.frame(statics)
  names(statics) = c("direction","max", "min", "mean", "sd", "mean_positive", "sd_positive")
  write.csv(statics,paste("./results_multidevices_angles_test_car/",tag,"_statics.csv",sep = ""))
  return (statics)
}
getDataStatic(singlePeriod,"car")


