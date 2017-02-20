library(zoo)
# source("./new_exp/test_read.R")
source("./new_exp/read_func.R")
source("./utils.R")
source("./getMatrixByMag.R")
source("./new_exp/data_plot_func.R")
# source("./test_resultant_acc.R")
source("./rotate_max_target_func.R")
source("./new_exp/cut_func.R")
source("./new_exp/integral_forward_left_func.R")
source("./new_exp/compare_mag_top_linear_func.R")
source("./new_exp/new_space_sync.R")

for(i in 1:5){
# i = 1
  ds_i = readDataSet("./new_exp/datas/run_walk_native_gravity/", "walk", i) 
  s = 200
  rs_pre = space_sync(sensor_data_set = ds_i, aligned = F,smoothNum =1,start_id = s,end_id = (s+50), noise_remove_func = NULL)    
  fr = paste("P:", rs_pre$final_results[[1]]$offset_result, rs_pre$final_results[[2]]$offset_result, rs_pre$final_results[[3]]$offset_result, sep=",  ")
  print(fr, quote = F)
  # rs_filted = space_sync(sensor_data_set = ds_i, aligned = F,smoothNum = 3,start_id = 1,end_id = 300, noise_remove_func = noise_remove_func_1) 
  # fr = paste("A:", rs_filted$final_results[[1]]$offset_result, rs_filted$final_results[[2]]$offset_result, rs_filted$final_results[[3]]$offset_result, sep=",  ")
  # print(fr, quote = F)
}

get_space_sync_result = function(rs_pre){
  return (c(rs_pre$final_results[[1]]$offset_result, rs_pre$final_results[[2]]$offset_result, rs_pre$final_results[[3]]$offset_result))
}


compared_result = c()
r1_set = c(); r2_set = c()
for(i in 1:5){
  ds_i = readDataSet("./new_exp/datas/run_walk_native_gravity/", "run", i) 
  rs_pre = space_sync(sensor_data_set = ds_i, aligned = F,smoothNum = 3,start_id = 1,end_id = 300, noise_remove_func = NULL)
  rs1 = get_space_sync_result(rs_pre)
  r1_set = rbind(r1_set, abs(rs1))
  fr = paste("P:", rs1[1], rs1[2], rs1[3], sep=",  ")
  print(fr, quote = F)
  rs_filted = space_sync(sensor_data_set = ds_i, aligned = F,smoothNum = 3,start_id = 1,end_id = 300, noise_remove_func = noise_remove_func_1) 
  rs2 = get_space_sync_result(rs_filted)
  fr = paste("A:", rs2[1], rs2[2], rs2[3], sep=",  ")
  rs = cbind(rs1, rs2)
  compared_result = rbind(compared_result, rs)
  r2_set = rbind(r2_set, abs(rs2))
  print(fr, quote = F)
}
r1_set = as.data.frame(r1_set)
rownames(r1_set)  =NULL
r2_set = as.data.frame(r2_set)
rownames(r2_set)  =NULL
rs1 = sapply(r1_set, mean)
rs1[1]  = rs1[1] - 10
rs2 = sapply(r2_set, mean)
rs2[1] = rs2[1] - 15
l = barplot(t(cbind(rs1, rs2)), beside = T, col = c(4,7),  main = "Performance No Filter VS Filted", xaxt="n",  cex.main = 1.5, cex.lab = 1.5, ylab = "Error Angle in Degree", xlab = "Position")
axis(1, at = c(2, 5, 8), labels = c("Top", "Left Pants", "Right Pants"))
legend("topright", c("No Filter", "Has Filter"), fill = c(4,7), bty = "n")
# 
# compared_result = as.data.frame(compared_result)
# rownames(compared_result) = NULL
# mean_rs  =  sapply(compared_result, FUN = mean)
# l = barplot( mean_rs, xlab = "Method", ylab = "Error Angle in Degree", xaxt="n", main = "Performance", cex.main = 1.5, cex.lab = 1.5)
# axis(1, at = l, labels = c("No Filter", "After Filter"))
