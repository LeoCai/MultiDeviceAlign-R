source("./utils.R")
source("./getMatrixByMag.R")
library(zoo)

top = read.csv("./datas/2_24_mag_test/top.csv");top = top[800:1300,]

globalAccMag= getGlobalAccByMag(top)
matplot(globalAccMag[,2],type="l")


top = read.csv("./datas/2_17/Top_1.csv");top = top[800:1000,]

globalAccMag= getGlobalAccByMag(top)
matplot(globalAccMag[,2],type="l")
