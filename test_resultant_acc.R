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


resultantPlot = function(run_top,run_legl,run_legr, plot_name,selected_id, writeFile = F){
  cp_run = cbind(run_top$ResultantAcc[selected_id] ,run_legl$ResultantAcc[selected_id], run_legr$ResultantAcc[selected_id])
  cp_run = rollapply(cp_run, 5,mean)
  if(writeFile){
    png(file = paste("./new_exp/imags/",plot_name,".png",sep = ""),width = 1024,height = 768)
  }
  matplot(cp_run, type="l", main = plot_name,lwd = 2,lty=1,cex.main = 2,cex.lab = 1.5,ylab = "accerometer(g/m^2)",xlab = "index(50HZ)")
  legend("topright",legend = c("top","legl","legr"),lty = 1,col = 1:3,cex = 2)
  if(writeFile){
    dev.off()
  }
}
resultantPlot(run_top,run_legl,run_legr, "run_resultant_accerometer",c(1:80),T)
resultantPlot(walk_top,walk_legl,walk_legr, "walk_resultant_accerometer",c(100:250),T)
resultantPlot(upjump_top,upjump_legl,upjump_legr, "upjump_resultant_accerometer",c(100:250),T)
resultantPlot(forjump_top,forjump_legl,forjump_legr, "forwardjump_resultant_accerometer",c(0:300),T)



