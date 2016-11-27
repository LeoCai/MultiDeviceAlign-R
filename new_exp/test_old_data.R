library(zoo)
source("./new_exp/old_data.R")
source("./utils.R")
source("./getMatrixByMag.R")
source("./new_exp/data_plot_func.R")
# source("./test_resultant_acc.R")
source("./new_exp/new_space_sync.R")
library(pracma)


info_walk_1 = space_sync(readSample(1, 1), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = F)
info_walk_1 = space_sync(readSample(1, 2), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)
info_walk_1 = space_sync(readSample(1, 3), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)
info_walk_1 = space_sync(readSample(1, 4), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)

info_walk_1 = space_sync(readSample(2, 1), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)
info_walk_1 = space_sync(readSample(2, 2), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)
info_walk_1 = space_sync(readSample(2, 3), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)
info_walk_1 = space_sync(readSample(2, 4), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)

info_walk_1 = space_sync(readSample(3, 1), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)
info_walk_1 = space_sync(readSample(3, 2), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)
info_walk_1 = space_sync(readSample(3, 3), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)
info_walk_1 = space_sync(readSample(3, 4), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)

info_walk_1 = space_sync(readSample(4, 1), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)
info_walk_1 = space_sync(readSample(4, 2), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)
info_walk_1 = space_sync(readSample(4, 3), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)
info_walk_1 = space_sync(readSample(4, 4), start_id = 150, end_id = 800, smoothNum = 5, isPrePCA = T)

head(sensor_data_set_1[[1]])
