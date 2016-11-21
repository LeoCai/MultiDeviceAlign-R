library(zoo)
source("./new_exp/test_read.R")

resultantPlot = function(sensor_data_set, plot_name, selected_id, writeFile = F){
  cp_action_diff_pos = c()
  for(i in 1:length(sensor_data_set)){
    sensor_data = sensor_data_set[[i]][selected_id,]
    resultant_linear_acc = sqrt(sensor_data$LinearAcc0^2 + sensor_data$LinearAcc1^2 + sensor_data$LinearAcc2^2)
    cp_action_diff_pos = cbind(cp_action_diff_pos, resultant_linear_acc)
  }
  cp_action_diff_pos = rollapply(cp_action_diff_pos, 3, mean)
  if(writeFile){
    png(file = paste("./new_exp/imags/",plot_name,".png",sep = ""),width = 1024,height = 768)
  }
  matplot(cp_action_diff_pos, type="l", main = plot_name,lwd = 2,lty=1,cex.main = 2,cex.lab = 1.5,ylab = "accerometer(g/m^2)",xlab = "index(50HZ)")
  legend("topright",legend = c("top","legl","legr"),lty = 1,col = 1:3,cex = 2)
  if(writeFile){
    dev.off()
  }
}

resultantPlot(list(walk_top,walk_legl,walk_legr), "WALK_RESULTANT_ACCEROMETER",c(400:550),T)
resultantPlot(list(run_top,run_legl,run_legr), "RUN_RESULTANT_ACCEROMETER",c(100:200),T)
resultantPlot(list(forjump_top,forjump_legl,forjump_legr), "FORJUMP_RESULTANT_ACCEROMETER",c(300:450),T)



