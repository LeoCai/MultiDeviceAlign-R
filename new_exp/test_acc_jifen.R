library(zoo)
source("./new_exp/test_read.R")
source("./utils.R")
source("./getMatrixByMag.R")
source("./new_exp/data_plot_func.R")
# source("./test_resultant_acc.R")
source("./rotate_max_target_func.R")
# source("./new_exp/new_space_sync.R")

ds1 = readDataSet("./datas/3_9_data_3path/",action = 1, expNum = 2)
top_data = ds1[[1]]
magData = cbind(top_data$ConvertedData0, top_data$ConvertedData1, top_data$ConvertedData2)
liData = cbind(top_data$LinearAcc0, top_data$LinearAcc2, -top_data$LinearAcc1)
gyr_data = cbind(top_data$Gyro0, top_data$Gyro1, top_data$Gyro2)
resultant_acc = sqrt(liData[,1]^2 + liData[,2]^2 + liData[,3]^2)
resultant_gyr = sqrt(gyr_data[,1]^2 + gyr_data[,2]^2 + gyr_data[,3]^2)
par(mfrow = c(2,1))
matplot(magData[1:100,], type = "l")
matplot(liData[1:100,], type = "l")
matplot(gyr_data[1:200,], type = "l")
par(mfrow = c(3,1))
plot(resultant_acc[200:400], type = "l")
plot(resultant_gyr[200:400], type="l")
plot(gyr_data[200:400,2], type="l")



matplot(magData[100:500,1:2], type="l")


boxplot_acc = function(expNum, fileName, writeFile = F){
  ds1 = readDataSet("./new_exp/datas/run_bulding_north/","run",expNum)
  magData = cbind(-ds1[[1]]$ConvertedData0, -ds1[[1]]$ConvertedData1, ds1[[1]]$ConvertedData2)
  magData = as.data.frame(magData)
  names(magData) = c("RIGHT", "FORWARD", "DOWN")
  print("integralAcc")
  # matplot(magData[,1:2], type="l")
  int_1_pos = integralAcc(integralAcc(magData[,1], ds1[[1]]$dt), ds1[[1]]$dt)
  int_2_pos = integralAcc(integralAcc(magData[,2], ds1[[1]]$dt), ds1[[1]]$dt)
  int_1_v = integralAcc(magData[,1], ds1[[1]]$dt)
  int_2_v = integralAcc(magData[,2], ds1[[1]]$dt)
  if(writeFile)
    png(filename = fileName, width = 1024, height = 1024)
  par(mfrow = c(2,1))
  matplot(cbind(int_1_v, int_2_v), type="l", main = "VELOSITY", ylim = c(-3,6), cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5, xlab = "index(50HZ)", ylab = "m/s")
  legend("topright", legend = c("RIGHT","FORWARD"), col = 1:2 , lty = 1)
  matplot(cbind(int_1_pos, int_2_pos), type="l", main = "POSITION",  ylim = c(-20,50), cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5, xlab = "index(50HZ)", ylab = "m")
  legend("topright", legend = c("RIGHT","FORWARD"), col = 1:2 , lty = 1)
  if(writeFile)
    dev.off()
}

for(i in 1:10){
  boxplot_acc(i, paste("./new_exp/imags/","integral_", i, ".png", sep=""), T)
}


plot_integrated = function(acc, dt, title){
  int_1_pos = integralAcc(acc[,1], dt)
  int_2_pos = integralAcc(acc[,2], dt)
  ratio = abs( int_1_pos[length(int_1_pos)] ) / (abs( int_2_pos[length(int_2_pos)])+1)
  # print(ratio)
  matplot(cbind(int_1_pos, int_2_pos), type="l", main = title, lty = 1)
}

for( i in 1){
  ds1 = readDataSet("./new_exp/datas/run_bulding_north/","run", 1)
  lacc = cbind(-ds1[[1]]$ConvertedData0, -ds1[[1]]$ConvertedData1, ds1[[1]]$ConvertedData2)
  par(mfrow = c(2,1))
  rs = rotateMaxTarget(lacc, ds1[[1]]$dt, targetFunc_v_sd, plotOut = T)
  angle = rs$max_angle
  rotated_lacc = rotateHorizentalByAngle(lacc, angle)
  plot_integrated(lacc, ds1[[1]]$dt, "BEFORE ROTATE POSITION")
  plot_integrated(rotated_lacc, ds1[[1]]$dt, "AFTER ROTATE POSITION")
  
  
  
  # plot_integrated(rotated_lacc, ds1[[1]]$dt, "AFTER ROTATE POSITION")
}



