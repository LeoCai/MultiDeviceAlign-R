library(zoo)
source("./new_exp/test_read.R")
source("./utils.R")
source("./getMatrixByMag.R")
source("./test_resultant_acc.R")
source("./new_exp/new_space_sync.R")

info_walk = space_sync(list(walk_top, walk_legr, walk_legl), start_id = 150, end_id = 800, smoothNum = 5)
aligned_data_set = alignData(list(walk_top, walk_legr, walk_legl), start = 150, end = 800)

matplot(cbind(aligned_data_set[[2]]$ResultantAcc[50:200], aligned_data_set[[3]]$ResultantAcc[50:200]),type= "l")

resultantPlot(-info_walk$smothed_aligned_data_set, "RUN_RESULTANT_ACCEROMETER",c(20:80),F)

cor_fc_walk = round(cor(info_walk$FC, -info_walk$smothed_aligned_data_set[[1]]$ConvertedData1),2)
plotFcTop(info_walk$FC, -info_walk$smothed_aligned_data_set[[1]]$ConvertedData1, paste("FC_TOP_WALK", cor_fc_walk,sep="_"), T)
plotSingleData(info_walk$FC, "FC_WALK",T)

info_run = space_sync(list(run_top, run_legr, run_legl), smoothNum = 3)
cor_fc_run = round(cor(info_run$FC, info_run$smothed_aligned_data_set[[1]]$ConvertedData1),2)
plotFcTop(info_run$FC, info_run$smothed_aligned_data_set[[1]]$ConvertedData1, paste("FC_TOP_RUN", cor_fc_run,sep="_"), T)
plotSingleData(info_run$FC, "FC_RUN",T)

info_upjump = space_sync(list(upjump_top, upjump_legr, upjump_legl), smoothNum = 3)
cor_fc_upjump = round(cor(info_upjump$FC, -info_upjump$smothed_aligned_data_set[[1]]$ConvertedData1),2)
plotFcTop(info_run$FC, -info_upjump$smothed_aligned_data_set[[1]]$ConvertedData1, paste("FC_TOP_UPJUMP", cor_fc_upjump,sep="_"), T)
plotSingleData(info_upjump$FC, "FC_UP_JUMP",T)

info_forjump = space_sync(list(forjump_top, forjump_legr, forjump_legl), end = 800, smoothNum = 3)
aligned_data_set = alignData(list(forjump_top, forjump_legr, forjump_legl), end = 800)
matplot(cbind(aligned_data_set[[2]]$ResultantAcc, aligned_data_set[[3]]$ResultantAcc),type= "l")

cor_fc_forjump = round(cor(info_forjump$FC, info_forjump$smothed_aligned_data_set[[1]]$ConvertedData1),2)
plotFcTop(info_forjump$FC, info_forjump$smothed_aligned_data_set[[1]]$ConvertedData1, paste("FC_TOP_FORJUMP", cor_fc_forjump,sep="_"), T)
plotSingleData(info_forjump$FC, "FC_FORWARD_JUMP",T)

# sensor_data_set =list(forjump_top,forjump_legl,forjump_legr )
# aligned_data = alignData(sensor_data_set)
# fc1 = computeFC(aligned_data)
