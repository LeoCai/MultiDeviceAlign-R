computeFC = function(sensor_data_set, isPrePCA = F){
  pcaData = c()
  horizental_data_set = list()
  for(i in 1:length(sensor_data_set)){
    sensor_data = sensor_data_set[[i]]; firstData = sensor_data[1,]
    gvInit = cbind(firstData$Gravity0, firstData$Gravity1, firstData$Gravity2)
    mgInit = cbind(firstData$MagnetData0, firstData$MagnetData1, firstData$MagnetData2)
    initRotationMatirx = getMatrixByMag(gvInit,mgInit)
    globalAccs = getGlobalAccByMag(sensor_data)
    globalAccs[,3] = 0; #proj on horizontal
    rotatedGlobal = c()
    for (j in 1:nrow(globalAccs)) {
      rotatedAcc = rotationByMatrix(globalAccs[j,],initRotationMatirx)
      rotatedGlobal = rbind(rotatedGlobal, rotatedAcc)
    }
    horizental_data_set[[i]] = rotatedGlobal
  }
  # if(isPrePCA){
  #   horizental_data_set = prePCAProcess(horizental_data_set)
  # }
  for(i in 1:length(horizental_data_set)){
    rotatedGlobal = horizental_data_set[[i]]
    for(j in 1:3){  pcaData = cbind(pcaData, rotatedGlobal[,j]);  }
  }
  epca6A <- prcomp(pcaData, center = F, scale. = F)
  # print(epca6A$sdev^2)
  # plot(epca6A$sdev,type = "l" ,main = "egvalue")
  newData = predict(epca6A, newdata = pcaData)
  # have some problem here
  # pcaForward = -newData[,1]
  pcaForward = newData[,1]
  if(abs(min(pcaForward)) < abs(max(pcaForward))) pcaForward = -pcaForward
  plot(pcaForward[50:250],type="l",main="Fc")
  return(pcaForward)
}

smoothByFFt = function(sensor_data_set, ratio){
  for(i in 1:length(sensor_data_set)){
    sensor_data = sensor_data_set[[i]]
    for(j in 1:ncol(sensor_data)){
      ds = sensor_data[,j]
      L = length(ds)/2+1
      ids = (L-ratio*L):(L + ratio*L)
      fft_ds = fft(ds)
      fft_ds[ids] = 0
      sensor_data[,j] = abs(ifft(fft_ds))
    }
    sensor_data_set[[i]] = sensor_data
  }
  return(sensor_data_set)
}

prePCAProcess = function(sensor_data_set, removeIndex = 2){
  #remove left right using pca
  first_data = list()
  first = FALSE
  processed_data = list()
  for( i in 1:length(sensor_data_set)){
    sensor_data = sensor_data_set[[i]]
    epca6A <- prcomp(sensor_data, center = F, scale. = F)
    newData = predict(epca6A, newdata = sensor_data)
    if(!first) {
      first_data = newData;
      first = TRUE
    }
    else {
      # reverse direction
      # print("cor pre pca")
      # print(cor(newData[1:100,1], sensor_data[1:100,1]))
      # print(cor(newData[1:100,2], sensor_data[1:100,2]))
      if(cor(newData[1:50,removeIndex],first_data[1:50,removeIndex])<0){ newData = -newData }
    }
    # matplot(newData, type="l")
    newData[,c(removeIndex,3)] = 0
    removed_data = t(t(newData %*% t(epca6A$rotation)) + epca6A$center)
    processed_data[[i]] = removed_data
  }
  return(processed_data)
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
  # matplot(1:length(data1_selected),cbind(data1_selected, data2_selected),type = "l",main = paste(tag,round(maxCor, 4)))
  return (best_start2:best_end2)
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

syncByFc = function(horizental_acc, Fc, tag = "", plot3d = F) {
  #just use hroizontal vector
  #compare result vector to c(0, 1, 0)
  #1. compute Fc vector
  #2. compute angle
  # horizental_acc = getGlobalAccByMag(sensor_data)
  # horizental_acc[,3] = 0
  Fi = horizental_acc; Fc_direction = c();
  
  for (dim in 1:3) {
    EFi = mean(Fi[,dim]); EFc = mean(Fc); 
    # print(paste(EFi,EFc))
    meanR = EFi / EFc; Fc_direction = c(Fc_direction,meanR)
  }
  Fc_vector = c()
  for (i in 1:length(Fc)) { Fc_vector = rbind(Fc_vector, Fc[i] * unitVector(Fc_direction)) }
  # print(paste("uni_direction",unitVector(Fc_direction)))
  
  Fi_noise = Fc_vector - Fi
  Fi_noise = as.data.frame(Fi_noise)
  names(Fi_noise) = c("Finx","Finy","Finz")
  
  Fc_Magnitude  = sqrt(Fc_vector[,1] ^ 2 + Fc_vector[,2] ^ 2 + Fc_vector[,3] ^ 2)
  Fi_noise_Magnitude = sqrt(Fi_noise[,1] ^ 2 + Fi_noise[,2] ^ 2 + Fi_noise[,3] ^ 2)
  # print(paste("Fc_direction", Fc_direction))
  angle = computeAngle(Fc_direction, c(0,1,0))
  sig = getDirection(Fc_direction, c(0,1,0), c(0,0,1))
  if(angle > 90 && angle < 180) angle = 180-angle
  else if(angle < -90  && angle > - 180) angle = -180 - angle
  
  print(sig * angle)
  
  if (plot3d) {
    init3d(tag); myArrow3d(Fc_direction * 4,2)
    for (i in 1:nrow(horizental_acc)) { myArrow3d(horizental_acc[i,]); }
  }
  return (list(Fc_direction= Fc_direction, offset_result =sig*angle))
}

getHorizentalAcc = function(sensor_data_set){
  global_data_set = list()
  for(i in 1:length(sensor_data_set)){
    sensor_data = sensor_data_set[[i]]
    # globalAccs = getGlobalAccByMag(sensor_data)
    globalAccs = cbind(sensor_data$ConvertedData0, sensor_data$ConvertedData1, sensor_data$ConvertedData2)
    globalAccs[,3] = 0
    global_data_set[[i]] = globalAccs
    # print("cor horizental top")
    # print(cor(globalAccs[1:100,1], sensor_data_set[[1]][1:100,]$LinearAcc2))
    # print(cor(globalAccs[1:100,2], sensor_data_set[[1]][1:100,]$LinearAcc2))
  }
  return(global_data_set)
}

space_sync = function(sensor_data_set, smoothNum = 3, start_id = 50, end_id = 300, isPrePCA = F, removeIndex = 2){
  # sensor_data_set = smoothByFFt(sensor_data_set, ratio = 0.8)
  sensor_data_set = sensorSmooth(sensor_data_set, smooth_size = smoothNum)
  aligned_data_set = alignData(sensor_data_set, start = start_id, end = end_id)
  # smothed_data_set = sensorSmooth(aligned_data_set, smooth_size = smoothNum)
  fc = computeFC(aligned_data_set, isPrePCA = isPrePCA)
  totalInfo = list()
  final_results = list()
  horizental_acc = getHorizentalAcc(aligned_data_set)
  if(isPrePCA){
    horizental_acc = prePCAProcess(horizental_acc, removeIndex)
  }
  for(i in 1:length(horizental_acc)){
    sync_result = syncByFc(horizental_acc[[i]], fc)
    # print(sync_result)
    final_results[[i]] = sync_result
  }
  print("")
  totalInfo$final_results = final_results
  totalInfo$FC = fc
  totalInfo$smothed_aligned_data_set = horizental_acc
  return(totalInfo)
}
# ds1 = readDataSet("./new_exp/datas/run_bulding_north/","run",2)
info_walk = space_sync(ds1, start_id = 150, end_id = 500, smoothNum = 5,isPrePCA = T, removeIndex = 2)
# info_walk = space_sync(ds1, start_id = 150, end_id = 500, smoothNum = 5,isPrePCA = T)

# info_walk = space_sync(ds1, start_id = 150, end_id = 500, smoothNum = 5,isPrePCA = F)

# info_walk_1 = space_sync(readSample(1, 1), start_id = 150, end_id = 800, smoothNum = 3)


# info_walk = space_sync(list(walk_top, walk_legr, walk_legl), start_id = 150, end_id = 800, smoothNum = 5)
