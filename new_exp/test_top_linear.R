library(zoo)
source("./new_exp/data_plot_func.R")
source("./new_exp/test_read.R")


plotLinear = function(sensor_data, selected_id, file_name, writeFile = F){
  sensor_data = sensor_data[selected_id, ]
  linear_data = cbind(sensor_data$LinearAcc0, sensor_data$LinearAcc1, sensor_data$LinearAcc2)
  if(writeFile){
    png(file = paste("./new_exp/imags/",file_name,".png",sep = ""),width = 1024,height = 768)
  }
  matplot(linear_data, main = file_name, type = "l", col = c(3,2,1), lwd = 2, lty=c(3,2,1), cex.main = 2, cex.lab = 1.5, ylab = "accerometer(g/m^2)", xlab = "index(50HZ)")
  legend("topright",legend = c("RIGHT","DOWN","FORWARD"), col = c(3,2,1),lty=c(3,2,1), cex = 2)
  if(writeFile){
    dev.off()
  }
}

plotLinear(-walk_top, selected_id = 50:300, "WALK_TOP_LINEAR", F)
plotLinear(-run_top, selected_id = 50:150, "RUN_TOP_LINEAR", T)
plotLinear(-upjump_top, selected_id = 50:350, "UP_JUMP_TOP_LINEAR", T)
plotLinear(-forjump_top, selected_id = 50:350, "FORWARD_JUMP_TOP_LINEAR", T)

# plotLinear(-walk_top, selected_id = 50:300, "WALK_TOP_LINEAR", F)
plotLinearData(-walk_top,  selected_id = 50:300, "WALK_TOP_LINEAR", c("RIGHT","DOWN","FORWARD"), F)
