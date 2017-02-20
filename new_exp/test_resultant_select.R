source("./new_exp/read_func.R")


ds = readDataSet("./new_exp/datas/run_walk_native_gravity/", "walk", 1)
top_data = ds[[1]]
rs_reultant = sqrt(top_data$LinearAcc0^2 + top_data$LinearAcc1^2 + top_data$LinearAcc2^2)
plot(-top_data$ConvertedData2[200:400], type= "l")
