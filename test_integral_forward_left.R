library(zoo)
source("./new_exp/read_func.R")
source("./utils.R")
source("./getMatrixByMag.R")
source("./new_exp/data_plot_func.R")
# source("./test_resultant_acc.R")
source("./rotate_max_target_func.R")
source("./new_exp/cut_func.R")
source("./new_exp/integral_forward_left_func.R")
source("./new_exp/compare_mag_top_linear_func.R")



expId = 5
action = "walk"
ds1 = readDataSet("./new_exp/datas/run_walk_native_gravity/",action = action, expNum = expId)

rs = compare_mag_toplc_gyro(ds1, 1:200, "mag_toplc_gyro_compare", F)
magData = rs$mag_global; liData  = rs$top_linear; top_data = ds1[[2]]

magData = cbind(top_data$ConvertedData1, -top_data$ConvertedData0, -top_data$ConvertedData2)
liData = cbind(top_data$LinearAcc2, -top_data$LinearAcc0, top_data$LinearAcc1)
gyr_data = cbind(top_data$Gyro0, top_data$Gyro1, top_data$Gyro2)

# test_slice_integral(liData[,1], liData[,2], gyr_data, 1:100, top_data$dt)
test_slice_integral(magData[,1], magData[,2], gyr_data, 1:100, top_data$dt, paste("INTEGRAL_FORWARD_LEFT",action,expId, sep="_"), F)

# rs = rotateMaxTarget(liData[1:100,], top_data$dt[1:100], targetFunc_pos, T )
rs = rotateMaxTarget(magData, top_data$dt, targetFunc_pos, T, paste("ANGLE_TO_OBJECTIVE",action,expId, sep="_"), T )

