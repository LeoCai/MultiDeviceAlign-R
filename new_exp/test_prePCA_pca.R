library(zoo)
source("./new_exp/test_read.R")
source("./utils.R")
source("./getMatrixByMag.R")
source("./new_exp/data_plot_func.R")
# source("./test_resultant_acc.R")
source("./new_exp/new_space_sync.R")


ds1 = readDataSet("./new_exp/datas/run_bulding_north/","run",7)
processed_rs = space_sync(ds1,smoothNum = 5,start_id = 50, end_id = 500,isPrePCA = F, removeIndex = 1)

processs_linear_rs = processed_rs$smothed_aligned_data_set[[1]]
processs_linear_rs = as.data.frame(processs_linear_rs)
names(processs_linear_rs) = c("LinearAcc0", "LinearAcc1", "LinearAcc2")

par(mfrow = c(3,1))
magData = cbind(-ds1[[1]]$ConvertedData0,ds1[[1]]$ConvertedData1,ds1[[1]]$ConvertedData2)
magData = as.data.frame(magData)
summary(magData)
names(magData) = c("LinearAcc0", "LinearAcc1", "LinearAcc2")
png(filename = "./new_exp/imags/TOP_PREPCA_PLOT_COMPARE.png", width = 1024, height = 1024)
par(mfrow = c(3,1))
plotLinearData(magData, 50:249, c("RIGHT","FORWARD","DOWN"), c(3,1,2), "TOP_MAG_GLOBAL", F)
plotLinearData(ds1[[1]], 50:249, c("RIGHT","DOWN","FORWARD"), c(3,2,1), "TOP_LINEAR", F)
plotLinearData(processs_linear_rs,  1:200, c("RIGHT","FORWARD","DOWN"), c(3,1,2), "TOP_LINEAR_AFTER_PCA_REMOVE", F)
dev.off()
k = processed_rs$smothed_aligned_data_set[[1]]

predata = ds1[[1]][50:249,3]
process1 = processs_linear_rs[1:200,1]
process2 = processs_linear_rs[1:200,2]
cor(process1, process2)
