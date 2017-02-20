source("./utils.R")
source("alg_tracked_selected_mean.R")
source("alg_mean.R")
source("alg_tracked_mean.R")
source("gacc_gyr_track.R")

LACC_INDEXES = 1:3
GACC_INDEXES = 4:6
GYR_INDEXES = 7:9
MAG_INDEXES = 10:12
GLOBAL_MAG_INDEXES = 13:15
DT_INDEXES = 16

readMultiSensorData = function(path){
  datas = read.csv(path, header = F)
  return(datas)
}
getClientNum= function(multi_data){
  return(ncol(multi_data)/16)
}
getClientData = function(multi_data, i){
  return(multi_data[, (i*16+1): ((i+1)*16)])
}

computeAngleAndSimilarity = function(fc_all, multi_data, clientNum){
  angle_all = c(); similarity_all = c()
  for (i in 1:clientNum){
    client_data = getClientData(multi_data, i-1)
    gacc_data = client_data[,GACC_INDEXES]
    global_data = client_data[,GLOBAL_MAG_INDEXES]
    mag_data = client_data[,MAG_INDEXES]
    angle = computeAngleByMagGaccFC(mag_data[1,], gacc_data[1,], fc_all[i,])
    init_x = fc_all[i,]
    # similarity = computeSimilarity(client_data, init_x)
    angle_all = c(angle_all, angle)
    # similarity_all = c(similarity_all, similarity)
  }
  return(list(angle_all, similarity_all))
}

computeAngle = function(v1, v2){
  sum = 0
  for(i in 1:3){
    sum = sum + v1[i]*v2[i]
  }
  cosAngle = sum / (absVector(v1)*absVector(v2))
  sign = 1
  return(sign*acos(cosAngle)*180/pi)
}

adjustAngle = function(angle){
  if(angle>180) angle = angle - 360
  if(angle > 90 && angle < 180) angle = 180-angle
  else if(angle < -90  && angle > - 180) angle = -180 - angle
  return(angle)
}

computeAngleByMagGaccFC = function(mag_data, gacc_data, fc){
  rtm_mag = getRotationMatrixG2BBy2Vectors(gacc_data, mag_data)
  rtm_fc = getRotationMatrixG2BBy2Vectors(gacc_data, fc)
  print(paste("mg:", rtm_mag[,2]))
  print(paste("fc:", rtm_fc[,2]))
  angle = computeAngle(rtm_mag[,2], rtm_fc[,2])
  angle = adjustAngle(angle)
  return(angle)
}

computeSimilarity = function(client_data, init_x){
  lacc_data = client_data[,LACC_INDEXES]
  gyr_data = client_data[,GYR_INDEXES]
  dt_data = client_data[,DT_INDEXES]
  gacc_data = client_data[,GACC_INDEXES]
  global_data = client_data[,GLOBAL_MAG_INDEXES]
  tracked_data = trackData(gacc_data, gyr_data, dt_data, lacc_data, init_x)
  rs_cor = 0
  for(i in 1:3){
    rs_cor = rs_cor + abs(cor(tracked_data[,i], global_data[,i]))
  }
  return(rs_cor/3)
}

multi_data = readMultiSensorData("./exp_final/datas/Test6/multi_data_02_20_15_04_13.csv")
clientNum = getClientNum(multi_data)
head(multi_data[,MAG_INDEXES])
# rs_mean = algMean(multi_data, clientNum)
rs_tracked_mean = algTrackedSelectedMean(multi_data, clientNum, F)
rs_tracked_selected_mean = algTrackedSelectedMean(multi_data, clientNum, T)

(angleSim = computeAngleAndSimilarity(rs_tracked_selected_mean, multi_data, clientNum))
(angleSim = computeAngleAndSimilarity(rs_tracked_mean, multi_data, clientNum))

