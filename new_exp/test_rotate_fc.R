source("./new_exp/read_func.R")
source("./new_exp/new_space_sync.R")
library(dtw)

rotate_fc_dtw = function(expNum){
  ds = readDataSet("./new_exp/datas/run_walk_native_gravity/", "walk", expNum)
  start = 100; end = 400
  selected_index = start:end
  aligned_set = alignData(ds,start = start, end = end)
  fc = computeFC(aligned_set, isPrePCA = F)
  print(paste("cor fc", cor(ds[[1]]$LinearAcc2[selected_index], fc)))
  # matplot(cbind(fc, -ds[[1]]$LinearAcc2[selected_index]), type="l")
  # d = dtw(fc, -ds[[1]]$LinearAcc2[selected_index])
  # d$distance
  
  par(mfrow = c(3,1))
  for(k in 1:length(ds)){
    rs = c()
    for( i in seq(0, 2*pi, 0.1)){
      sensor_data = ds[[k]]
      dt = rotateHorizentalByAngle(cbind(sensor_data$ConvertedData1, sensor_data$ConvertedData0, sensor_data$ConvertedData2), i)
      rs = c(rs, dtw(fc, dt[selected_index,1])$distance )
    }
    angle_seq = seq(0, 2*pi, 0.1)/pi*180
    plot(x = angle_seq, y = rs, type="l", xaxt="n")
    axis(1, round(seq(0, pi, 0.1)/pi*180))
    final_angle = angle_seq[which(rs == min(rs))] 
    print(adjustAngle(final_angle))
  }
  par(mfrow = c(1,1))
}

for(i in 1:5)
rotate_fc_dtw(i)


for(i in 1:5){
  ds = readDataSet("./new_exp/datas/run_walk_native_gravity/", "run", i)
  start = 1; end = 80
  rs = space_sync(ds, start_id = start, end_id = end)
  rs = rs$final_results
  for(j in 1:length(rs)){
    print(rs[[j]]$offset_result)
  }
}


