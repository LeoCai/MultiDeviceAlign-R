
algTrackedMean = function(multi_data, clientNum){
  fc_all = c()
  for (i in 1:clientNum){
    client_data = getClientData(multi_data, i-1)
    lacc_data = client_data[,LACC_INDEXES]
    gyr_data = client_data[,GYR_INDEXES]
    dt_data = client_data[,DT_INDEXES]
    gacc_data = client_data[,GACC_INDEXES]
    tracked_data = trackData(gacc_data, gyr_data, dt_data, lacc_data)
    rtm_g2b = getRotationMatrixG2BBy2Vectors(gacc_data[1,], init_x)
    fc = getGlobalData(fc, t(rtm_g2b))
    fc_all = rbind(fc_all, fc)
  }
  return(fc_all)
}
