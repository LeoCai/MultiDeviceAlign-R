library(zoo)
source("./new_exp/read_func.R")
source("./utils.R")
source("./getMatrixByMag.R")
source("./new_exp/data_plot_func.R")
# source("./test_resultant_acc.R")
source("./new_exp/new_space_sync.R")



for(i in 1:10){
  ds1 = readDataSet("./new_exp/datas/run_bulding_north/","run",i)
  info_walk = space_sync(ds1, start_id = 150, end_id = 500, smoothNum = 5,isPrePCA = F)
}
