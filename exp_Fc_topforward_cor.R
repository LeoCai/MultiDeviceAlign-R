source("./utils.R")
source("./timealign_3_2.R")
source("./getMatrixByMag.R")
source("./addPCAData.R")
source("./plot_debug.R")
library(zoo)

selectIndex = 1:699
topGlobal = getGlobalAccByMag(top)
topforward = topGlobal[selectIndex,2]
write.csv(topforward,"./results_fc_topforward_cor/topforward.csv")

Fc = computeFc()


corFcForward = cor(Fc,topforward)
matplot(cbind(topforward,Fc),type="l",main = paste("Fc by MagnitudePCA  topforward cor:", round(corFcForward,3)),cex.main=1.5,cex.lab=1.5,ylab="magnitude")
legend("topright",c("topforward","Fc by MagnitudePCA"),lty =1:2,col=1:2 )
write.csv(Fc,"./results_fc_topforward_cor/FC_Magnitude.csv")

addDeviceDataToPCA = addDeviceDataToPCA2
Fc = computeFc()
corFcForward = cor(Fc,topforward)
matplot(cbind(topforward,Fc),type="l",main = paste("Fc by VectorPCA topforward cor:", round(corFcForward,3)),cex.main=1.5,cex.lab=1.5,ylab="magnitude")
legend("topright",c("topforward","Fc by VectorPCA"),lty =1:2,col=1:2 )
write.csv(Fc,"./results_fc_topforward_cor/FC_Vector.csv")

# plot(topforward,type="l",main="top forward")

# cor(Fc,topGlobal[selectIndex,2])

