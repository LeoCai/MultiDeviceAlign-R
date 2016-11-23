fileParent = "3_9_data_3path"
readSample = function(path, i){
  #读取样本，把top_data作为基准
  top_data = read.csv(paste("./datas/",fileParent,"/top/", path, i,".csv",sep = ""))
  leftHand_data = read.csv(paste("./datas/",fileParent,"/lefthand/", path, i,".csv",sep = ""))
  rightHand_data = read.csv(paste("./datas/",fileParent,"/righthand/", path, i,".csv",sep = ""))
  leftPants_data = read.csv(paste("./datas/",fileParent,"/leftpants/", path, i,".csv",sep = ""))
  rightPants_data = read.csv(paste("./datas/",fileParent,"/rightpants/", path, i,".csv",sep = ""))
  
  sensor_data_set = list(top_data, leftHand_data, rightHand_data, leftPants_data, rightPants_data)
  return(sensor_data_set)
  
}