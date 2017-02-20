library(fastICA)
library(TSA)
source("./new_exp/read_func.R")
source("./new_exp/new_space_sync.R")


walk_set = readDataSet("./new_exp/datas/run_walk_native_gravity/", "walk", 2)

top_data = walk_set[[2]]
selected_index = 100:400
li_data = cbind(top_data$LinearAcc0, top_data$LinearAcc1, top_data$LinearAcc2)
mag_data = cbind(top_data$ConvertedData0,top_data$ConvertedData1, top_data$ConvertedData2)
comp = fastICA(li_data[selected_index,], n.comp = 2)
matplot(comp$S, type="l")
mag_data = mag_data[selected_index,]
cor(comp$S[,2], mag_data[,2])
plot(mag_data[,2] , type="l")
matplot(cbind(mag_data[,2], comp$S[,1]), type="l")
cor(mag_data[,2], comp$S[,1])
cor(li_data[selected_index,3],mag_data[,2])
periodogram(comp$S[,1])
periodogram(comp$S[,2])
periodogram(mag_data[,2])


rs = fft(li_data[,1])
length(rs)
len = length(li_data[,1])
y = rs[2:(floor(len/2)+1)]
par(mfrow = c(2,1))
plot(x = 1:length(y)/50,y = sqrt(Re(y)^2 + Im(y)^2), type="l")
periodogram(li_data[,1])
