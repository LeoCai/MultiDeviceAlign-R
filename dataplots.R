library(zoo)

source("../handshake_9_11/model/GloableConvert.R")

writePlot = F

dataSam = read.csv("./datas/SensorData1_SAM.csv",colClasses = rep("numeric"))
dataMi = read.csv("./datas/SensorData1_MI.csv",colClasses = rep("numeric"))

head(dataSam)



# nv = normVector(dataMi$Gravity0,dataMi$Gravity1,dataMi$Gravity2)
# absVector(nv[,1],nv[,2],nv[,3])

computeProjOnGravaty = function(data) {
  absProj = ((
    data$LinearAcc0 * data$Gravity0 + data$LinearAcc1 * data$Gravity1 + data$LinearAcc2 * data$Gravity2
  ) /
    absVector(data$Gravity0, data$Gravity1, data$Gravity2)
  )
  nvg = normVector(data$Gravity0, data$Gravity1, data$Gravity2)
  return(cbind(nvg * absProj,absProj))
}

computeProjOnHori = function(data) {
  pg = computeProjOnGravaty(data)
  l1 = data$LinearAcc0; l2 = data$LinearAcc1; l3 = data$LinearAcc2;
  return(cbind(l1 - pg[,1],l2 - pg[,2],l3 - pg[,3]))
}



computAndPlotData = function(fileHead,fileTag,fileEnd, start, end) {
  data = readData(fileHead,fileTag,fileEnd, start, end)
  l1 = data$LinearAcc0; l2 = data$LinearAcc1; l3 = data$LinearAcc2
  g1 = data$Gravity0; g2 = data$Gravity1; g3 = data$Gravity2;
  gyr1 = data$Gyro0; gyr2 = data$Gyro1;  gyr3 = data$Gyro2;
  
  if (writePlot)
    png(
      filename = paste("./imgs/",fileTag,"_Vector",".png",sep = ""), width = 1360, height = 768
    )
  par(mfrow = c(3,1))
  matplot(
    x = 1:nrow(data),y = cbind(l1,l2,l3),type = "l" ,lwd = 1.5, xlab = "index",ylab = "magnitude", main = paste("Linear Accelerometer",fileTag) ,cex.lab = 1.5, cex.main = 2
  )
  legend(
    "topright",c("x","y","z"),lty = 1:3, col = 1:3,cex = 1.5
  )
  matplot(
    x = 1:nrow(data),y = cbind(g1,g2,g3),type = "l",lwd = 1.5,xlab = "index",ylab = "magnitude",main = paste("Gravity",fileTag) ,cex.lab = 1.5, cex.main = 2
  )
  legend(
    "topright",c("x","y","z"),lty = 1:3, col = 1:3,cex = 1.5
  )
  matplot(
    x = 1:nrow(data),y = cbind(gyr1,gyr2,gyr3),type = "l",lwd = 1.5,xlab = "index",ylab = "magnitude",main = paste("Groscope",fileTag) ,cex.lab = 1.5, cex.main = 2
  )
  legend(
    "topright",c("x","y","z"),lty = 1:3, col = 1:3,cex = 1.5
  )
  if (writePlot)
    dev.off()
  
  pg = computeProjOnGravaty(data)
  ph = computeProjOnHori(data)
  abspg = absVector(pg[,1],pg[,2],pg[,3])
  absph = absVector(ph[,1],ph[,2],ph[,3])
  abslacc = absVector(l1, l2, l3)
  abslacc = rollapply(abslacc,3, mean)
  abspg = rollapply(abspg,3, mean)
  absph = rollapply(absph,3, mean)
  if (writePlot)
    png(
      filename = paste("./imgs/",fileTag,"_Resultant",".png",sep = ""), width = 1360, height = 768
    )
  par(mfrow = c(3,1))
  plot(
    abslacc,type = "l", main = paste("Resultant Linear Accelerometer",fileTag) ,lwd = 1.5,cex.lab = 1.5, cex.main = 2, ylab = "magnitude", ylim = c(0,5)
  )
  plot(
    abspg,type = "l", main = paste("Resultant Projection OnGravity Direction",fileTag), lwd = 1.5,cex.lab = 1.5,cex.main = 2, ylab = "magnitude", ylim = c(0,5)
  )
  plot(
    absph,type = "l", main = paste("Resultant Projection On Horizontal Direction",fileTag),lwd = 1.5,cex.lab = 1.5, cex.main = 2, ylab = "magnitude", ylim = c(0,5)
  )
  if (writePlot)
    dev.off()
}

computAndPlotData("./datas/SensorData","1_MI",".csv",350,500)
computAndPlotData("./datas/SensorData","2_MI",".csv",350,500)
computAndPlotData("./datas/SensorData","3_MI",".csv",350,500)
computAndPlotData("./datas/SensorData","1_SAM",".csv",350,500)
computAndPlotData("./datas/SensorData","2_SAM",".csv",350,500)
computAndPlotData("./datas/SensorData","3_SAM",".csv",350,500)







compareGlobalAccs = function(expNum,s1,e1,s2,e2){
  data1 = readData("./datas/SensorData",paste(expNum,"MI",sep = "_"),".csv",s1,e1)
  data2 = readData("./datas/SensorData",paste(expNum,"SAM",sep = "_"),".csv",s2,e2)
  globalAccs1 = convertBaseOnFt(data1)
  # print(globalAccs1)
  
  globalAccs2 = convertBaseOnFt(data2)
  if (writePlot)
    png(
      filename = paste("./imgs/","GlobalVector_",expNum,".png",sep = ""), width = 1360, height = 768
    )
  par(mfrow = c(3,1))
  for(i in 1:3){
    matplot(1:nrow(data1), cbind(globalAccs1[,i],globalAccs2[,i]), xlab = "index",ylab = "magnitude",lwd = 1.5 , cex.lab = 1.5, type = "l")
    if(i == 1) title(main = paste("Global Accelerometer",expNum), cex.main = 1.5) 
    legend(
      "topright",c("device1","device2"),lty = 1:2, col = 1:2,cex = 1.5
    )
  }
  if (writePlot)
    dev.off()
  if (writePlot)
    png(
      filename = paste("./imgs/","Magnitude_",expNum,".png",sep = ""), width = 1360, height = 768
    )
  par(mfrow = c(1,1))#main = cor(data1$ResultantAcc,data2$ResultantAcc)
  matplot(1:nrow(data1), cbind(data1$ResultantAcc,data2$ResultantAcc),lwd = 1.5 ,xlab = "index",ylab = "magnitude", cex.lab = 1.5, type = "l", main = paste("Resultant Linear Accelerometer",expNum), cex.main = 1.5)
  legend(
    "topright",c("device1","device2"),lty = 1:2, col = 1:2,cex = 1.5
  )
  if (writePlot)
    dev.off()
}

compareGlobalAccs(1, 400,500,398,498)

compareGlobalAccs(2, 400,500,380,480)

compareGlobalAccs(3, 400,500,390,490)







# plot(dataSam[200:400,1],type = "l")
# plot(dataMi[200:400,1],type = "l")
#
# matplot(cbind(dataSam[300:800,1],dataMi[305:800,1]),type = "l")
# matplot(cbind(dataSam[300:800,2],dataMi[305:800,2]),type = "l")
# matplot(cbind(dataSam[300:800,3],dataMi[305:800,3]),type = "l")
#
# legend("topright",c("data1","data2"),lty = 1:2, col = 1:2)
