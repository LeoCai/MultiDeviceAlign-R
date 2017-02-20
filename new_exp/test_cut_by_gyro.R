library(zoo)
source("./new_exp/test_read.R")
source("./utils.R")
source("./getMatrixByMag.R")
source("./new_exp/data_plot_func.R")
# source("./test_resultant_acc.R")
source("./rotate_max_target_func.R")
source("./new_exp/integral_forward_left_func.R")
source("./new_exp/compare_mag_top_linear_func.R")
# source("./new_exp/new_space_sync.R")



ds1 = readDataSet("./new_exp/datas/run_bulding_north/",action = "run", expNum = 4)
rs = compare_mag_toplc_gyro(ds1, 200:400, "mag_toplc_gyro_compare", F)
magData = rs$mag_global
liData  = rs$top_linear
top_data = ds1[[1]]
magData = cbind(top_data$ConvertedData0, top_data$ConvertedData1, top_data$ConvertedData2)
liData = cbind(top_data$LinearAcc0, top_data$LinearAcc2, -top_data$LinearAcc1)
gyr_data = cbind(top_data$Gyro0, top_data$Gyro1, top_data$Gyro2)
resultant_acc = sqrt(liData[,1]^2 + liData[,2]^2 + liData[,3]^2)
resultant_gyr = sqrt(gyr_data[,1]^2 + gyr_data[,2]^2 + gyr_data[,3]^2)
par(mfrow = c(2,1))
matplot(magData[1:100,], type = "l")
matplot(liData[1:100,], type = "l")
matplot(gyr_data[1:200,], type = "l")
par(mfrow = c(3,1))
plot(resultant_acc[200:400], type = "l")
plot(resultant_gyr[200:400], type="l")
plot(gyr_data[200:400,2], type="l")
# par(mfrow = c(2,1))
# temp_gyr_z = gyr_data[,2]
# threhold = 3
# temp_gyr_z[abs(temp_gyr_z)<threhold&abs(temp_gyr_z)>-threhold] = 0
# plot(gyr_data[1:500,2], type="l")
# plot(temp_gyr_z[1:500], type="l")


filt_small_acc = function(acc, threhold){
  acc[abs(acc)<threhold&abs(acc)>-threhold] = 0
  return(acc)
}
# filted_mag_data_f = filt_small_acc(magData[,2], 4)
# filted_mag_data_l = filt_small_acc(magData[,1], 4)
# matplot(filted_mag_data_f, type="l")
# matplot(filted_mag_data_l, type="l")


filted_mag_data_f = liData[,1]
filted_mag_data_l = liData[,2]

# cutIndex = cutByGryo(gyr_data[,2],threhold = 3, off_ratio_start = 5, off_ratio_end = 3)
# 
# total_rs = c()
# for(i in 1:length(cutIndex)){
#   cutId = cutIndex[[i]]; s = cutId[1]; e = cutId[2]
#   rs = test_slice_integral(filted_mag_data_f, filted_mag_data_l, s:e, "RUN_SINGLE_PERIOD_TOPLI_INTEGRATE_1", F)
#   total_rs = c(total_rs, as.integer(rs[1]>rs[2]))
# }
# print(sum(total_rs) / length(total_rs))

# test_slice_integral(filted_mag_data_f, filted_mag_data_l, cutIndex[[1]][1]:cutIndex[[1]][2], "RUN_SINGLE_PERIOD_TOPLI_INTEGRATE_1", F)


test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 1:100, top_data$dt, "RUN_SINGLE_PERIOD_TOPLI_INTEGRATE_1", F)

test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 98:128, top_data$dt, "RUN_SINGLE_PERIOD_TOPLI_INTEGRATE_1", T)
test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 98:128-33, top_data$dt, "RUN_SINGLE_PERIOD_TOPLI_INTEGRATE_2", T)
test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 98:128+30, top_data$dt, "RUN_SINGLE_PERIOD_TOPLI_INTEGRATE_3", T)
test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 98:128+66, top_data$dt, "RUN_SINGLE_PERIOD_TOPLI_INTEGRATE_4", F)
test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 98:128+132, top_data$dt, "RUN_SINGLE_PERIOD_TOPLI_INTEGRATE_5", F)
test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 98:128+164, top_data$dt, "RUN_SINGLE_PERIOD_TOPLI_INTEGRATE_6", F)


filted_mag_data_f = magData[,1]
filted_mag_data_l = magData[,2]
test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 96:128, top_data$dt, "RUN_SINGLE_PERIOD_INTEGRATE_1", T)
test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 96:128-33, top_data$dt, "RUN_SINGLE_PERIOD_INTEGRATE_2", T)
test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 96:128+30, top_data$dt, "RUN_SINGLE_PERIOD_INTEGRATE_3", T)
test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 96:130+65, top_data$dt, "RUN_SINGLE_PERIOD_INTEGRATE_4", F)
test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 96:128+131, top_data$dt, "RUN_SINGLE_PERIOD_INTEGRATE_5", F)
test_slice_integral(filted_mag_data_f, filted_mag_data_l, gyr_data, 96:128+164, top_data$dt, "RUN_SINGLE_PERIOD_INTEGRATE_6", F)



