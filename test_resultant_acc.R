library(zoo)
run_legl = read.csv("./new_exp/datas/leg_l/run.csv")
forjump_legl = read.csv("./new_exp/datas/leg_l/forjump.csv")
upjump_legl = read.csv("./new_exp/datas/leg_l/upjump.csv")
walk_legl = read.csv("./new_exp/datas/leg_l/walk.csv")

run_legr = read.csv("./new_exp/datas/leg_r/run.csv")
forjump_legr = read.csv("./new_exp/datas/leg_r/forjump.csv")
upjump_legr = read.csv("./new_exp/datas/leg_r/upjump.csv")
walk_legr = read.csv("./new_exp/datas/leg_r/walk.csv")

run_top = read.csv("./new_exp/datas/top/run.csv")
forjump_top = read.csv("./new_exp/datas/top/forjump.csv")
upjump_top = read.csv("./new_exp/datas/top/upjump.csv")
walk_top = read.csv("./new_exp/datas/top/walk.csv")

upjump_split = read.csv("./new_exp/Upjump2.csv")
forjump_split = read.csv("./new_exp/forjump.csv")


resultantPlot(run_top,run_legl,run_legr, "run_resultant_accerometer",c(1:80),T)
resultantPlot(walk_top,walk_legl,walk_legr, "walk_resultant_accerometer",c(100:250),T)
resultantPlot(upjump_top,upjump_legl,upjump_legr, "upjump_resultant_accerometer",c(100:250),T)
resultantPlot(forjump_top,forjump_legl,forjump_legr, "forwardjump_resultant_accerometer",c(0:300),T)



