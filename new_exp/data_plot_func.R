resultantPlot = function(run_top,run_legl,run_legr, plot_name,selected_id, writeFile = F){
  cp_run = cbind(run_top$ResultantAcc[selected_id] ,run_legl$ResultantAcc[selected_id], run_legr$ResultantAcc[selected_id])
  cp_run = rollapply(cp_run, 5,mean)
  if(writeFile){
    png(file = paste("./new_exp/imags/",plot_name,".png",sep = ""),width = 1024,height = 768)
  }
  matplot(cp_run, type="l", main = plot_name,lwd = 2,lty=1,cex.main = 2,cex.lab = 1.5,ylab = "accerometer(g/m^2)",xlab = "index(50HZ)")
  legend("topright",legend = c("top","legl","legr"),lty = 1,col = 1:3,cex = 2)
  if(writeFile){
    dev.off()
  }
}

plotSingleData = function(fc,fileName,writeFile = F){
  if(writeFile){ png(paste("./new_exp/imags/",fileName,".png",sep=""), width = 1024, height = 768) }
  plot(fc, type = "l", main = fileName, cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5,xlab = "index(50HZ)", ylab = "accerometer")
  if(writeFile){ dev.off() }
}

plotData = function(fc_top, fileName, legendName, writeFile=F ){
  if(writeFile){ png(paste("./new_exp/imags/",fileName,".png",sep=""), width = 1024, height = 768) }
  matplot(fc_top, type="l", main = fileName, cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5,xlab = "index(50HZ)", ylab = "accerometer")
  legend("topright",legend = legendName,lty = 1,col = 1:3,cex = 2)
  if(writeFile){ dev.off() }
}

plotFcTop = function(fc, top, fileName, writeFile = F){
  fc_top = cbind(fc, top)
  legendName = c("FC","TOP")
  plotData(fc_top, fileName, legendName, writeFile)
}

plotLinearData = function(sensor_data, selected_id, legend, col, fileName, writeFile = F){
  sensor_data = sensor_data[selected_id, ]
  linear_data = cbind(sensor_data$LinearAcc0, sensor_data$LinearAcc1, sensor_data$LinearAcc2)
  if(writeFile){
    png(file = paste("./new_exp/imags/",fileName,".png",sep = ""),width = 1024,height = 768)
  }
  matplot(linear_data, main = fileName, type = "l", ylim = c(-15,15), col = col, lwd = 2, lty=1, cex.main = 2, cex.lab = 1.5, ylab = "accerometer(g/m^2)", xlab = "index(50HZ)")
  legend("topright",legend = legend, col =col,lty=1, cex = 2)
  if(writeFile){
    dev.off()
  }
}