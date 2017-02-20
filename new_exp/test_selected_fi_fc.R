source("./new_exp/read_func.R")

source("./new_exp/new_space_sync.R")


sensor_data = readDataSet("./new_exp/datas/run_walk_native_gravity/", "walk", 1)

aligned_data = alignData(sensor_data,start = 100)
rs = computeFC(aligned_data)

k = getGlobalAccByMag(aligned_data[[1]])

k[,3] = 0
selected_Index = rs>0
matplot(k[selected_Index,] ,type="l")
mean(k[selected_Index,1])


matplot(-k[,2],type = "l")
matplot(aligned_data[[1]]$LinearAcc2,type = "l")

matplot(k[,3],type = "l")
matplot(aligned_data[[1]]$LinearAcc1,type = "l")

matplot(k[,1],type = "l")
matplot(aligned_data[[1]]$LinearAcc0,type = "l")

# for(i in 1:length(aligned_data)){
# }

par(mfrow = c(2,1))
plot(rs[2:30],type="l")
matplot(cbind(rs[2:30], aligned_data[[1]][2:30,6]), type="l")
matplot(cbind(rs,-aligned_data[[1]][,4:6]), type="l")
legend("topleft", legend = c("fc", "left","gravity", "forward"), lty = 1:4, col=1:4)

