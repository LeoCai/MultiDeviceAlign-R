library(zoo)
source("./new_exp/test_read.R")
source("./utils.R")
source("./getMatrixByMag.R")
source("./new_exp/data_plot_func.R")
# source("./test_resultant_acc.R")
source("./rotate_max_target_func.R")
source("./new_exp/cut_func.R")

ds1 = readDataSet("./new_exp/datas/run_bulding_north/",action = "run", expNum = 1)
top_data = ds1[[1]]
magData = cbind(top_data$ConvertedData0, top_data$ConvertedData1, top_data$ConvertedData2)
liData = cbind(top_data$LinearAcc0, top_data$LinearAcc2, -top_data$LinearAcc1)
gyr_data = cbind(top_data$Gyro0, top_data$Gyro1, top_data$Gyro2)


cutIndexes = cutByGryo(gyr_data[,2], threhold = 3)


