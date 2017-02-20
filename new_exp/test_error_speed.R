

# sensor_data = read.csv("./new_exp/datas/error_exp/w11.csv")
# forward_acc = sensor_data$LinearAcc2
# t = sum(sensor_data$dt)
# a_real = (7.2*2) / (t^2)
# mean(forward_acc)
# plot(forward_acc, type="l")
writeFile = T
if(writeFile) png(paste("./new_exp/imags/", "accerometer_walk_no_line", ".png", sep = ""), width = 1024, height = 1024)
fileName = paste("./new_exp/datas/error_exp/","www", 2, ".csv", sep="")
sensor_data = read.csv(fileName)
len = nrow(sensor_data)
forward_acc = -sensor_data$LinearAcc2
plot(forward_acc, type="l", main = "accerometer walk", xlab = "speed degree", ylab = "accerometer" , cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5)
abline(h= -0.5, col =2)
if(writeFile) dev.off()

computeMeanAndRealAcc = function(pre,expNum, threhold = -0.5){
  fileName = paste("./new_exp/datas/error_exp/",pre, expNum, ".csv", sep="")
  sensor_data = read.csv(fileName)
  len = nrow(sensor_data)
  forward_acc = -sensor_data$LinearAcc2
  # plot(forward_acc, type="l")
  t = sum(sensor_data$dt)
  a_real = (7.2*2) / (t^2)
  if(is.null(threhold)) return(c(mean(forward_acc), a_real))
  else return(c(mean(forward_acc[forward_acc>threhold]), a_real))
}


walk_rs = c()
for(sp in 1:4){
  for(expNum in 1:5){
    rs = computeMeanAndRealAcc("w",sp*10+expNum)
    walk_rs = rbind(walk_rs, rs)
  }
}
for(i in 1:10){
  rs = computeMeanAndRealAcc("ww", i)
  walk_rs = rbind(walk_rs, rs)
}
for(i in 1:55){
  rs = computeMeanAndRealAcc("www", i)
  walk_rs = rbind(walk_rs, rs)
}

tag = "Measured_to_Real_Accerometer_WALK"
writeFile = T
if(writeFile) png(paste("./new_exp/imags/", tag, ".png", sep = ""), width = 1024, height = 1024)
walk_rs_orderd = walk_rs[order(walk_rs[,2]),]
matplot(walk_rs_orderd, type="l", main = tag, xlab = "speed degree", ylab = "accerometer" , cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5)
legend("topleft", c("Measured", "Real"), col = 1:2, lty = 1 ,cex = 2) 
if(writeFile) dev.off()

plotByScal = function(walk_rs_orderd, scaleNum = 3, tag = "", writeFile = F){
  max_num = max(walk_rs_orderd[,2])
  min_num = min(walk_rs_orderd[,2])
  acc_scale = (max_num-min_num)/scaleNum
  row.names(walk_rs_orderd) = NULL
  walk_rs_orderd = as.data.frame(walk_rs_orderd)
  walk_rs_orderd[,3] = as.factor(round((walk_rs_orderd[,2]-min_num) / acc_scale))
  walk_rs_orderd[,4] = walk_rs_orderd[,1] - walk_rs_orderd[,2]
  names(walk_rs_orderd) = c("test","real","aeroid","delta")
  u_acc = aggregate(test~aeroid, data = walk_rs_orderd, FUN = mean)
  sd_acc = aggregate(delta~aeroid, data = walk_rs_orderd, FUN = sd)
  u_sd = cbind(u_acc[,2],sd_acc[,2])
  # par(mfrow = c(2,1))
  if(writeFile) png(paste("./new_exp/imags/", tag, ".png", sep = ""), width = 1024, height = 1024)
  plot(u_sd, type="p", xlim = c(0,2), ylim = c(0, 0.8), main = paste("u to sd ", scaleNum), xlab = "mean speed", ylab = "error", pch = 20, cex = 5, cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5)
  curve(0.12*x^2+0.08, 0, 2, add = T, col = "red", lwd = 2)
  if(writeFile) dev.off()
}

plotByScal(walk_rs_orderd, 3, "error_u_to_sd_3", F)
plotByScal(walk_rs_orderd, 4, "error_u_to_sd_4", F)
plotByScal(walk_rs_orderd, 5, "error_u_to_sd_5", F)
plotByScal(walk_rs_orderd, 6, "error_u_to_sd_6", F)
plotByScal(walk_rs_orderd, 7, "error_u_to_sd_7", F)
plotByScal(walk_rs_orderd, 8, "error_u_to_sd_8", F)
plotByScal(walk_rs_orderd, 9, "error_u_to_sd_9", F)
plotByScal(walk_rs_orderd, 10, "error_u_to_sd_10", F)
plotByScal(walk_rs_orderd, 11, "error_u_to_sd_11", F)
plotByScal(walk_rs_orderd, 12, "error_u_to_sd_12", F)
plotByScal(walk_rs_orderd, 13, "error_u_to_sd_13", F)
plotByScal(walk_rs_orderd, 14, "error_u_to_sd_14", F)
plotByScal(walk_rs_orderd, 15, "error_u_to_sd_15", T)



run_rs = c()
for(sp in 5){
  for(expNum in 1:5){
    rs = computeMeanAndRealAcc("w",sp*10+expNum, threhold = 0)
    run_rs = rbind(run_rs, rs)
  }
}
for(i in 11:18){
  rs = computeMeanAndRealAcc("ww", i, threhold = 0)
  run_rs = rbind(run_rs, rs)
}
tag = "Measured_to_Real_Accerometer_RUN"
writeFile = T
if(writeFile) png(paste("./new_exp/imags/", tag, ".png", sep = ""), width = 1024, height = 1024)
run_rs_orderd = run_rs[order(run_rs[,2]),]
matplot(run_rs_orderd, type="l", main = tag, xlab = "speed degree", ylab = "accerometer" , cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5)
legend("topleft", c("Measured", "Real"), col = 1:2, lty = 1 ,cex = 2) 
if(writeFile) dev.off()
