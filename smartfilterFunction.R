smartFilter = function(data,maxThrehold= 1,tag = "", mylim = c(-1.5,1),th = 0.8){
  lowpassLacc_forward = rollapply(data,4,mean)
  print(mean(lowpassLacc_forward))
  
  plot(lowpassLacc_forward,type = "l",main=paste(tag,"acc lowpass with Peek"),ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5)
  peakIndexs = c()
  for(i in 2:(length(lowpassLacc_forward)-1)){
    if((lowpassLacc_forward[i]>lowpassLacc_forward[i-1]&&lowpassLacc_forward[i]>lowpassLacc_forward[i+1])
       ||(lowpassLacc_forward[i]<lowpassLacc_forward[i-1]&&lowpassLacc_forward[i]<lowpassLacc_forward[i+1])){
      peakIndexs = c(peakIndexs, i)
    }
  }
  filteredIndex = c()
  for(i in 2:(length(peakIndexs)-1)){
    mid = lowpassLacc_forward[peakIndexs[i]]; pre = lowpassLacc_forward[peakIndexs[i-1]]; nex = lowpassLacc_forward[peakIndexs[i+1]]
    if(
      ((mid-pre)>th&&(mid-nex)>th)||
      ((pre-mid)>th&&(nex-mid)>th)
    ){
      filteredIndex = c(filteredIndex, peakIndexs[i])
    }
  }
  points(x = peakIndexs, y = lowpassLacc_forward[peakIndexs], col = "green")
  points(x = filteredIndex, y = lowpassLacc_forward[filteredIndex], col = "red")
  
  
  
#   peakLacc_forward = rollapply(lowpassLacc_forward,3,function(x){
#     # print(x)
#     if(x[2]>x[1]&&x[2]>x[3]){
#       return(1);
#     }else if(x[2]<x[1]&&x[2]<x[3]){
#       return(-1);
#     }else return(0);
#   })
  
  filterLacc_forward_index = rep(0,length(lowpassLacc_forward))
  for(i in 1:length(lowpassLacc_forward)){
    if(abs(lowpassLacc_forward[i])>maxThrehold){
      filterLacc_forward_index[i] = 1
    }
  }
  plotFilterdLacc_forward = lowpassLacc_forward
  # plotFilterdLacc_forward[filterLacc_forward_index==1] = 0
  prePeak = 0
  findStop = F;
  length(lowpassLacc_forward)
  length(plotFilterdLacc_forward)
  for(i in 1:length(lowpassLacc_forward)){
    if(filterLacc_forward_index[i]==1){
      findStop = T
      # print("findStop")
    }
    if(peakLacc_forward[i]==1){
      if(findStop) {
        plotFilterdLacc_forward[prePeak:i] = 0
        prePeak = i
        findStop = F
      }else {
        prePeak = i
      }
    }
  }
  plot(plotFilterdLacc_forward, type = "l",
       main=paste(tag,"filted acc"),ylab="(acc m/s^2)",cex.main=1.5,cex.lab= 1.5,lwd = 1.5,ylim= mylim)
  return(plotFilterdLacc_forward)
}


source("preprocess.R")
source("../handshake_9_11/model/GloableConvert.R")
source("./utils.R")
source("./readData.R")
d = top[97:376,];globalAcc= cbind(d$ConvertedData0,d$ConvertedData1,d$ConvertedData2)
plot(globalAcc[,2],type="l")
smartFilter(globalAcc[,2],maxThrehold = 1,tag="top forward",mylim =c(-1,2),th=1.2)
smartFilter(globalAcc[,1],maxThrehold = 1,tag="top left right",mylim =c(-1,2),th=1.2)

d = leftHand[83:275,];globalAcc= cbind(d$ConvertedData0,d$ConvertedData1,d$ConvertedData2)
plot(globalAcc[,2],type="l")
smartFilter(globalAcc[,2],maxThrehold = 1,tag="lefthand forward",mylim =c(-1,2),th=1.2)
smartFilter(globalAcc[,1],maxThrehold = 1,tag="lefthand left right",mylim =c(-1,2),th=1.2)
