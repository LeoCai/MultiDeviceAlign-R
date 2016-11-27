integralAcc = function(accs, dt){
  kn = accs
  for(i in 2:length(accs)){
    sum = 0
    for(j in 2:i){
      sum = sum + (accs[j]+accs[j-1])*dt[j]/2
    }
    kn[i] = sum
  }
  return(kn)
}

rotateHorizentalByAngle = function(lacc, angle){
  #rotate from z by angle
  rtm = matrix(c(cos(angle), -sin(angle), 0, sin(angle), cos(angle), 0, 0, 0, 1), byrow = T, ncol = 3)
  mt_lacc = as.matrix(lacc)
  rotated_t_acc = rtm%*%t(mt_lacc)
  return(t(rotated_t_acc))
}

rotateMaxTarget = function(sensor_data, targetFunc){
  #ratate data to find max target
  max_target = 0
  max_angle = 0
  rs = c()
  lacc = cbind(sensor_data$ConvertedData0, sensor_data$ConvertedData1, sensor_data$ConvertedData2)
  angle_seq = seq(0, 2*pi, 0.1)
  max_lacc = c()
  for( angle in angle_seq){
    print(angle)
    rotated_lacc = rotateHorizentalByAngle(lacc, angle)
    target = targetFunc(rotated_lacc, sensor_data$dt)
    rs = c(rs, target)
    if(max_target < target){
      max_target = target; max_angle = angle; max_lacc = rotated_lacc
    }
  }
  angle_seq = angle_seq/pi*180
  plot(cbind(angle_seq,rs), type="l")
  return(list(max_angle = max_angle, max_lacc = max_lacc, max_target = max_target, records = rs))
}

targetFunc_v = function(rotated_acc, dt){
  v_arr_1 = integralAcc(rotated_acc[,1], dt)
  v_arr_2 = integralAcc(rotated_acc[,2], dt)
  return(abs( v_arr_1[length(v_arr_1)] ) / (abs( v_arr_2[length(v_arr_2)])+1) )
}

targetFunc_pos = function(rotated_acc, dt){
  v_arr_1 = integralAcc(integralAcc(rotated_acc[,1], dt), dt)
  v_arr_2 = integralAcc(integralAcc(rotated_acc[,2], dt), dt)
  return(abs( v_arr_1[length(v_arr_1)] ) / (abs( v_arr_2[length(v_arr_2)])+1) )
}