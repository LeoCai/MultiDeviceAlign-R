compare_mag_toplc_gyro = function(ds1, selected_index, tag = "", writeFile = F){
  top_data = ds1[[1]]
  magData = cbind(top_data$ConvertedData1, -top_data$ConvertedData0,  -top_data$ConvertedData2)
  liData = cbind(top_data$LinearAcc2, -top_data$LinearAcc0, top_data$LinearAcc1)
  gyr_data = cbind(top_data$Gyro0, top_data$Gyro1, top_data$Gyro2)
  resultant_acc = sqrt(liData[,1]^2 + liData[,2]^2 + liData[,3]^2)
  resultant_gyr = sqrt(gyr_data[,1]^2 + gyr_data[,2]^2 + gyr_data[,3]^2)
  if(writeFile) png(paste("./new_exp/imags/", tag, ".png", sep = ""), width = 1024, height = 1024)
  par(mfrow = c(3,1))
  matplot(magData[selected_index, ], type = "l", main="MAG GLOBAL" ,cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5, xlab = "index(50HZ)", ylab = "accerometer")
  legend("topright", c("forward", "right", "down"), col = 1:3,lty = 1, cex = 2)
  matplot(liData[selected_index, ], type = "l", main="LINEAR TOP",cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5, xlab = "index(50HZ)", ylab = "accerometer")
  legend("topright", c("forward", "right", "down"), col = 1:3,lty = 1, cex = 2)
  # matplot(gyr_data[1:200,], type = "l")
  plot(gyr_data[selected_index, 2], type="l", main = "GYROSCOPE AROUND GRIVITY", cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5, xlab = "index(50HZ)", ylab = "angle velocity")
  if(writeFile) dev.off()
  return(list(mag_global = magData, top_linear = liData))
}