library(zoo)
library(stats)
library(signal)

source("utils.R")
source("pcaprocess.R")

writePlot = T

multiUnit = function(data, indexes){
  k = data.frame()
  
  for(i in 1:nrow(data)){
    k = rbind(k,as.numeric(unitVector(as.numeric(data[i,indexes]))))
  }
  names(k) = c("X", "Y", "Z")
  return(k)
}

index = 1

plotMagGlobal = function(expIndex,expTag, s1, e1, s2, e2, s3, e3){
 # s1 = 60;e1 = 250;s2 = 57;e2 = 247;s3 = 65;e3 = 255
  file1 = paste("./datas/1_18_data/",expIndex,"_d1.csv",sep = "")
  file2 = paste("./datas/1_18_data/",expIndex,"_d2.csv",sep = "")
  file3 = paste("./datas/1_18_data/",expIndex,"_d3.csv",sep = "")
  d1 = read.csv(file1)
  d2 = read.csv(file2)
  d3 = read.csv(file3)
  nrow(d1)
  nrow(d2)
  nrow(d3)
  d1 = d1[s1:e1,]
  d2 = d2[s2:e2,]
  d3 = d3[s3:e3,]
  
  resacc1 = d1$ResultantAcc
  resacc2 = d2$ResultantAcc
  resacc3 = d3$ResultantAcc
  
  Y <- fft(c(resacc1,resacc2,resacc3))
  mag <- sqrt(Re(Y)^2+Im(Y)^2)
  # plot(1:(length(mag)/2),mag[1:(length(mag)/2)],type="l" ,main = expTag)
  bw = butter(2, W=1/3, type="low")
 
#   matplot(x = 1:nrow(d1), y = cbind(filterdResacc1,filterdResacc2,filterdResacc3),type = "l")
#   matplot(x = 1:nrow(d1), y = cbind(resacc1,resacc2,resacc3),type = "l")
  
  # return()
  
  linearAcc1 = multiUnit(d1,c(1,2,3));
  linearAcc2 = multiUnit(d2,c(1,2,3));
  linearAcc3 = multiUnit(d3,c(1,2,3));
  
  pcaprocess(paste(expTag,"LinearAcc"),linearAcc1,linearAcc2,linearAcc3)
  # return()
  
  epca6A <- prcomp(rbind(linearAcc1,linearAcc2,linearAcc3),
                   center = TRUE,
                   scale. = FALSE) 
  pcaAcc1 = predict(epca6A, newdata=linearAcc1)
  pcaAcc2 = predict(epca6A, newdata=linearAcc2)
  pcaAcc3 = predict(epca6A, newdata=linearAcc3)
  
  mag1 = multiUnit(d1,c(10,11,12));
  mag2 = multiUnit(d2,c(10,11,12));
  mag3 = multiUnit(d3,c(10,11,12));
  
  magGlobal1= multiUnit(d1,c(13,14,15));
  magGlobal2= multiUnit(d2,c(13,14,15));
  magGlobal3= multiUnit(d3,c(13,14,15));
  
  pcaprocess(paste(expTag,"MAG_GLOBAL"),magGlobal1,magGlobal2,magGlobal3,F)
  
  globalAccByGy1 = convertBaseOnFt(d1)
  globalAccByGy2 = convertBaseOnFt(d2)
  globalAccByGy3 = convertBaseOnFt(d3)
  
  pcaprocess(paste(expTag,"GYR_GLOBAL"),globalAccByGy1,globalAccByGy2,globalAccByGy3,F)
  
  return()
  
  title = c("EAST","NORTH","SKY")
  device = c("device1","device2","device3")
  mcol = c(2,3,1)
  mlty = c(2,3,1)
  
  if (writePlot)
    png(
      filename = paste("./imgs/1_18/",index,expTag,"_mag_global_resultant",".png",sep = ""), width = 1360, height = 768
    )
  index <<- index+1
  par(mfrow = c(1,1))
  da1 = filter(bw,resacc1)
  da2 = filter(bw,resacc2)
  da3 = filter(bw,resacc3)
  matplot(x = 1:nrow(d1), y = cbind(da1,da2,da3),type = "l",col = mcol,lty=mlty, ltw = 1.5, xlab = "index",ylab="magnitude",cex.lab=1.5,cex.main=1.5, 
          main = paste(expTag,"RESULTANT ACCELEROMETER","\ncorrelation",
                       round(cor(da1,da2),2),round(cor(da1,da3),2)))
  legend(
    "topright",device,col = mcol,lty=mlty,cex = 1
  )
  if (writePlot)
    dev.off()
  
  if (writePlot)
    png(
      filename = paste("./imgs/1_18/",index,expTag,"_body_frame_vector",".png",sep = ""), width = 1360, height = 768
    )
  index <<-index+1
  par(mfrow = c(3,1))
  title = c("X","Y","Z")
  for(i in 1:3){
    k = 0+i
    da1 = filter(bw,linearAcc1[,i]); da2 = filter(bw,linearAcc2[,i]); da3 = filter(bw,linearAcc3[,i])
    matplot(x = 1:nrow(d1), y = cbind(
      da1, da2, da3
      ),type = "l",col = mcol,lty=mlty, ltw = 1.5, xlab = "index",ylab="magnitude",cex.lab=1.5,cex.main=1.5, 
            main = paste(expTag,"body frame accelerometer",title[i],"\ncorrelation",
                         round(cor(da1,da2),2),
                         round(cor(da1,da3),2)
                         ))
    legend(
      "topright",device,col = mcol,lty=mlty,cex = 1
    )
  }
  if (writePlot)
    dev.off()
  
  if (writePlot)
    png(
      filename = paste("./imgs/1_18/",index,expTag,"_mag_vector",".png",sep = ""), width = 1360, height = 768
    )
  index <<-index+1
  par(mfrow = c(3,1))
  for(i in 1:3){
    k = 10+i
    da1 = filter(bw,mag1[,i]); da2 = filter(bw,mag2[,i]); da3=filter(bw,mag3[,i])
    matplot(x = 1:nrow(d1), y = cbind(
      da1, da2, da3
      ),type = "l",col = mcol,lty=mlty, ltw = 1.5, xlab = "index",ylab="magnitude",cex.lab=1.5,cex.main=1.5, 
            main = paste(expTag,"body frame magnet",title[i],"\ncorrelation",
                         round(cor(da1,da2),2),
                         round(cor(da1,da3),2)
            ))
    legend(
      "topright",device,col = mcol,lty=mlty,cex = 1
    )
  }
  if (writePlot)
    dev.off()
  
  if (writePlot)
    png(
      filename = paste("./imgs/1_18/",index,expTag,"_acc_gyr_global_vector",".png",sep = ""), width = 1360, height = 768
    )
  index <<-index+1
  par(mfrow = c(3,1))
  title = c("GRYVITY","LEFT-RIGHT","FORWARD-BACK")
  for(i in 1:3){
    k = 0+i
    da1 = globalAccByGy1[,i]; da2 = globalAccByGy2[,i]; da3=globalAccByGy3[,i]
    matplot(x = 1:nrow(d1), 
            y = cbind(da1, da2, da3),type = "l",col = mcol,lty=mlty, ltw = 1.5, xlab = "index",ylab="magnitude",cex.lab=1.5,cex.main=1.5, 
            main = paste(expTag,"acc_gyr_global",title[i],"\ncorrelation",
                         round(cor(da1,da2),2),
                         round(cor(da1,da3),2)
            ))
    legend(
      "topright",device,col = mcol,lty=mlty,cex = 1
    )
  }
  if (writePlot)
    dev.off()
  
  if (writePlot)
    png(
      filename = paste("./imgs/1_18/",index,expTag,"_acc_gyr_global_vector_lowpass",".png",sep = ""), width = 1360, height = 768
    )
  index <<-index+1
  par(mfrow = c(3,1))
  title = c("GRYVITY","LEFT-RIGHT","FORWARD-BACK")
  for(i in 1:3){
    k = 0+i
    da1 = filter(bw,globalAccByGy1[,i]); da2 = filter(bw,globalAccByGy2[,i]); da3=filter(bw,globalAccByGy3[,i])
    matplot(x = 1:nrow(d1), 
            y = cbind(da1, da2, da3),type = "l",col = mcol,lty=mlty, ltw = 1.5, xlab = "index",ylab="magnitude",cex.lab=1.5,cex.main=1.5, 
            main = paste(expTag,"acc_gyr_global_lowpass",title[i],"\ncorrelation",
                         round(cor(da1,da2),2),
                         round(cor(da1,da3),2)
            ))
    legend(
      "topright",device,col = mcol,lty=mlty,cex = 1
    )
  }
  if (writePlot)
    dev.off()
  
  if (writePlot)
    png(
      filename = paste("./imgs/1_18/",index,expTag,"_mag_global_acc_vector",".png",sep = ""), width = 1360, height = 768
    )
  index <<-index+1
  par(mfrow = c(3,1))
  title = c("EAST","NORTH","SKY")
  for(i in 1:3){
    k = 13+i
    da1 = magGlobal1[,i]; da2 = magGlobal2[,i]; da3=magGlobal3[,i]
    matplot(x = 1:nrow(d1),
            y = cbind(da1, da2, da3),type = "l",col = mcol,lty=mlty, ltw = 1.5, xlab = "index",ylab="magnitude",cex.lab=1.5,cex.main=1.5, 
            main = paste(expTag,"mag_global_acc",title[i],"\ncorrelation",
                         round(cor(da1,da2),2),
                         round(cor(da1,da3),2)
            ))
    legend(
      "topright",device,col = mcol,lty=mlty,cex = 1
    )
  }
  if (writePlot)
    dev.off()
  
  if (writePlot)
    png(
      filename = paste("./imgs/1_18/",index,expTag,"_mag_global_acc_vector_lowpass",".png",sep = ""), width = 1360, height = 768
    )
  index <<-index+1
  par(mfrow = c(3,1))
  title = c("EAST","NORTH","SKY")
  for(i in 1:3){
    k = 13+i
    da1 = filter(bw,magGlobal1[,i]); da2 = filter(bw,magGlobal2[,i]); da3=filter(bw,magGlobal3[,i])
    matplot(x = 1:nrow(d1),
      y = cbind(da1, da2, da3),type = "l",col = mcol,lty=mlty, ltw = 1.5, xlab = "index",ylab="magnitude",cex.lab=1.5,cex.main=1.5, 
            main = paste(expTag,"mag_global_acc_lowpass",title[i],"\ncorrelation",
                         round(cor(da1,da2),2),
                         round(cor(da1,da3),2)
            ))
    legend(
      "topright",device,col = mcol,lty=mlty,cex = 1
    )
  }
  if (writePlot)
    dev.off()
  
  
 
}

plotMagGlobal(1, "STEADY_WALK_INDOOR: ",60,200,55,195,55,195)
plotMagGlobal(10,"UNSTEADY_WALK_INDOOR: ",60,200,62,202,65,205)
# plotMagGlobal(4,"STEADY_WALK_OUTDOOR: ",60,200,52,192,63,203)
# plotMagGlobal(7,"UNSTEADY_WALK_OUTDOOR: ",60,200,52,192,58,198)

# plotMagGlobal(2,"UBSTEADY_WALK: ",60,200,65,205,65,205)





