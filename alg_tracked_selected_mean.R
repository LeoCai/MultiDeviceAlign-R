
algTrackedSelectedMean = function(multi_data, clientNum, selected = T){
  fc_all = c()
  if(selected){
    fc_magnitude = computeFC(multi_data,clientNum)
    selected_index = getSelectedFc(fc_magnitude)
  }
  for (i in 1:clientNum){
    client_data = getClientData(multi_data, i-1)
    lacc_data = client_data[,LACC_INDEXES]
    gyr_data = client_data[,GYR_INDEXES]
    dt_data = client_data[,DT_INDEXES]
    gacc_data = client_data[,GACC_INDEXES]
    
    gacc_init = gacc_data[1,]
    if(gacc_init[1]>=7) {global_x = c(0,1,0)} else{global_x = c(1,0,0)}
    tracked_data = trackData(gacc_data, gyr_data, dt_data, lacc_data, global_x)
    tracked_data[,3] = 0
    
    if(selected)
      fc = apply(tracked_data[selected_index, ],2, mean)
    else
      fc = apply(tracked_data,2, mean)
  
    rtm_g2b = getRotationMatrixG2BBy2Vectors(gacc_data[1,], global_x)
    fc = getGlobalData(fc, t(rtm_g2b))
    fc_all = rbind(fc_all, fc)
  }
  return(fc_all)
}

getSelectedFc = function(fc_magnitude){
  return(fc_magnitude>0.05)
}

computeFC = function(multi_data,clientNum){
  pcaData = c()
  for(i in 1:clientNum){
    client_data = getClientData(multi_data, i-1)
    lacc_data = client_data[,LACC_INDEXES]
    gyr_data = client_data[,GYR_INDEXES]
    dt_data = client_data[,DT_INDEXES]
    gacc_data = client_data[,GACC_INDEXES]
    gacc_init = gacc_data[1,]
    if(gacc_init[1]>=7) {global_x = c(0,1,0)} else{global_x = c(1,0,0)}
    tracked_data = trackData(gacc_data, gyr_data, dt_data, lacc_data, global_x)
    tracked_data[,3] = 0
    pcaData = cbind(pcaData, tracked_data)
  }
  epca6A <- prcomp(pcaData, center = F, scale. = F)
  # print(epca6A$sdev^2)
  # plot(epca6A$sdev,type = "l" ,main = "egvalue")
  newData = predict(epca6A, newdata = pcaData)
  # have some problem here
  # pcaForward = -newData[,1]
  pcaForward = newData[,1]
  return(pcaForward)
}


