library(zoo)
source("./new_exp/test_read.R")
source("./utils.R")
source("./getMatrixByMag.R")
source("./new_exp/data_plot_func.R")
# source("./test_resultant_acc.R")
source("./new_exp/new_space_sync.R")


integralAcc = function(accs, dt){
  kn = accs
  for(i in 2:length(accs)){
    sum = 0
    for(j in 2:i){
      sum = sum + (accs[j]+accs[j-1])*dt[j]/2
    }
    kn[i] = sum
  }
  return(kn)
}

boxplot_acc = function(expNum){
  ds1 = readDataSet("./new_exp/datas/run_bulding_north/","run",expNum)
  magData = cbind(-ds1[[1]]$ConvertedData0, -ds1[[1]]$ConvertedData1, ds1[[1]]$ConvertedData2)
  magData = as.data.frame(magData)
  names(magData) = c("RIGHT", "FORWARD", "DOWN")
  print("integralAcc")
  # matplot(magData[,1:2], type="l")
  int_1_pos = integralAcc(integralAcc(magData[,1], ds1[[1]]$dt), ds1[[1]]$dt)
  int_2_pos = integralAcc(integralAcc(magData[,2], ds1[[1]]$dt), ds1[[1]]$dt)
  int_1_v = integralAcc(magData[,1], ds1[[1]]$dt)
  int_2_v = integralAcc(magData[,2], ds1[[1]]$dt)
  par(mfrow = c(2,1))
  matplot(cbind(int_1_v, int_2_v), type="l", main = "VELOSITY", ylim = c(-3,6))
  legend("topright", legend = c("RIGHT","FORWARD"), col = 1:2 , lty = 1)
  matplot(cbind(int_1_pos, int_2_pos), type="l", main = "POSITION", lty = 1, ylim = c(-20,50))
  legend("topright", legend = c("RIGHT","FORWARD"), col = 1:2 , lty = 1)

}

# for(i in 1:10){
#   boxplot_acc(i)
# }



for(i in 1:10){
  ds1 = readDataSet("./new_exp/datas/run_bulding_north/","run", i)
  rs = rotateMaxTarget(ds1[[1]], targetFunc_pos)
}


