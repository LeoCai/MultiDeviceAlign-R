computeFC = function(sensor_data_set){
  pcaData = c()
  for(i in 1:length(sensor_data_set)){
    sensor_data = sensor_data_set[[i]]; firstData = sensor_data[1,]
    gvInit = cbind(firstData$Gravity0, firstData$Gravity1, firstData$Gravity2)
    mgInit = cbind(firstData$MagnetData0, firstData$MagnetData1, firstData$MagnetData2)
    initRotationMatirx = getMatrixByMag(gvInit,mgInit)
    globalAccs = getGlobalAccByMag(sensor_data)
    globalAccs[,3] = 0; #proj on horizontal
    rotatedGlobal = c()
    for (i in 1:nrow(globalAccs)) {
      rotatedAcc = rotationByMatrix(globalAccs[i,],initRotationMatirx)
      rotatedGlobal = rbind(rotatedGlobal, rotatedAcc)
    }
    for(i in 1:3){  pcaData = cbind(pcaData, rotatedGlobal[,i]);  }
  }
  epca6A <- prcomp(pcaData, center = F, scale. = F)
  print(epca6A$sdev^2)
  # plot(epca6A$sdev,type = "l" ,main = "egvalue")
  newData = predict(epca6A, newdata = pcaData)
  # have some problem here
  # pcaForward = -newData[,1]
  pcaForward = newData[,1]
  if(abs(min(pcaForward))<abs(max(pcaForward))) pcaForward = -pcaForward
  plot(pcaForward[50:250],type="l",main="Fc")
  return(pcaForward)
}

testAlign = function(data1, data2,s1,e1,tag = "") {
  c1 = sqrt(data1$LinearAcc0 ^ 2 + data1$LinearAcc1 ^ 2 + data1$LinearAcc2 ^ 2)
  c2 = sqrt(data2$LinearAcc0 ^ 2 + data2$LinearAcc1 ^ 2 + data2$LinearAcc2 ^ 2)
  data1_selected = c1[s1:e1]
  maxCor = 0; best_start2 = s1; best_end2 = e1; range = round(s1 / 2)
  for (i in 1:range) {
    s2 = s1 - range / 2 + i; e2 = e1 - range / 2 + i
    data2_selected = c2[s2:e2]
    cvdata = cor(data1_selected,data2_selected)
    if (cvdata > maxCor) { best_start2 = s2; best_end2 = e2; maxCor = cvdata}
  }
  data2_selected = c2[best_start2:best_end2]
  matplot(1:length(data1_selected),cbind(data1_selected, data2_selected),type = "l",main = paste(tag,round(maxCor, 4)))
  return (best_start2:best_end2)
}

appendList = function(old_list, new_data){
  old_list[[length(old_list)+1]] = new_data
  return(old_list)
}

alignData = function(sensor_data_set, start=50, end = 300 ){
  base_data = list()
  base_data[[1]] = sensor_data_set[[1]]
  for(i in 2:length(sensor_data_set)){
    sensor_data = sensor_data_set[[i]]
    alignedIds = testAlign(base_data[[1]], sensor_data, start, end)
    base_data = appendList(base_data, sensor_data[alignedIds,])
  }
  base_data[[1]] = sensor_data_set[[1]][start:end,]
  return(base_data)
}

sensorSmooth= function(sensor_data_set, smooth_size = 3){
  for(i in 1:length(sensor_data_set)){
    sensorData =  sensor_data_set[[i]]
    smoothed = rollapply(sensorData, smooth_size, mean,partial = T)
    sensor_data_set[[i]] = as.data.frame(smoothed)
  }
  return(sensor_data_set)
}

syncByFc = function(sensor_data, Fc, tag = "", plot3d = F) {
  #just use hroizontal vector
  #compare result vector to c(0, 1, 0)
  #1. compute Fc vector
  #2. compute angle
  globalAcc = getGlobalAccByMag(sensor_data)
  globalAcc[,3] = 0
  Fi = globalAcc; Fc_direction = c();
  
  for (dim in 1:3) {
    EFi = mean(Fi[,dim]); EFc = mean(Fc); 
    # print(paste(EFi,EFc))
    meanR = EFi / EFc; Fc_direction = c(Fc_direction,meanR)
  }
  Fc_vector = c()
  for (i in 1:length(Fc)) { Fc_vector = rbind(Fc_vector, Fc[i] * unitVector(Fc_direction)) }
  
  Fi_noise = Fc_vector - Fi
  Fi_noise = as.data.frame(Fi_noise)
  names(Fi_noise) = c("Finx","Finy","Finz")
  
  Fc_Magnitude  = sqrt(Fc_vector[,1] ^ 2 + Fc_vector[,2] ^ 2 + Fc_vector[,3] ^ 2)
  Fi_noise_Magnitude = sqrt(Fi_noise[,1] ^ 2 + Fi_noise[,2] ^ 2 + Fi_noise[,3] ^ 2)
  angle = computeAngle(Fc_direction, c(0,1,0))
  sig = getDirection(Fc_direction, c(0,1,0), c(0,0,1))
  
  print(sig * angle)
  
  if (plot3d) {
    init3d(tag); myArrow3d(Fc_direction * 4,2)
    for (i in 1:nrow(globalAcc)) { myArrow3d(globalAcc[i,]); }
  }
  return (list(Fc_direction= Fc_direction, offset_result =sig*angle))
}

space_sync = function(sensor_data_set, smoothNum = 3, start_id = 50, end_id = 300){
  aligned_data_set = alignData(sensor_data_set, start = start_id, end = end_id)
  smothed_data_set = sensorSmooth(aligned_data_set, smooth_size = smoothNum)
  fc = computeFC(smothed_data_set)
  totalInfo = list()
  final_results = list()
  for(i in 1:length(sensor_data_set)){
    sync_result = syncByFc(aligned_data_set[[i]], fc)
    final_results[[i]] = sync_result
  }
  totalInfo$final_results = final_results
  totalInfo$FC = fc
  totalInfo$smothed_aligned_data_set = smothed_data_set
  return(totalInfo)
}

plotSingleData = function(fc,fileName,writeFile = F){
  if(writeFile){ png(paste("./new_exp/imags/",fileName,".png",sep=""), width = 1024, height = 768) }
  plot(fc, type = "l", main = fileName, cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5,xlab = "index(50HZ)", ylab = "accerometer")
  if(writeFile){ dev.off() }
}


plotData = function(fc_top, fileName, legendName, writeFile=F ){
  if(writeFile){ png(paste("./new_exp/imags/",fileName,".png",sep=""), width = 1024, height = 768) }
  matplot(fc_top, type="l", main = fileName, cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5,xlab = "index(50HZ)", ylab = "accerometer")
  legend("topright",legend = legendName,lty = 1,col = 1:3,cex = 2)
  if(writeFile){ dev.off() }
}

plotFcTop = function(fc, top, fileName, writeFile = F){
  fc_top = cbind(fc, top)
  legendName = c("FC","TOP")
  plotData(fc_top, fileName, legendName, writeFile)
}