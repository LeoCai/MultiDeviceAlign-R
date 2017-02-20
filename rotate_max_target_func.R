integralAcc = function(accs, dt){
  kn = rep(0, length(accs))
  for(i in 2 : length(accs)){
    kn[i] = kn[i-1]
    kn[i] = kn[i-1] + (accs[i]+accs[i-1])*dt[i]/2
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

rotateMaxTarget = function(lacc, dt, targetFunc, plotOut = F, tag="", writeFile = F){
  #ratate data to find max target
  max_target = 0
  max_angle = 0
  rs = c()
  angle_seq = seq(0, 2*pi, 0.1)
  max_lacc = c()
  for( angle in angle_seq){
    # print(angle)
    
    rotated_lacc = rotateHorizentalByAngle(lacc, angle)
    # if(angle == 5.9){
    #   print(angle)
    #   test_slice_integral(rotated_lacc[,1], rotated_lacc[,2], 1:100, dt)
    # }
    target = targetFunc(rotated_lacc, dt)
    # print(target)
    rs = c(rs, target)
    if(max_target < target){
      max_target = target; max_angle = angle; max_lacc = rotated_lacc
    }
  }
  angle_seq = angle_seq/pi*180
  if(plotOut)
  {
    if(writeFile) png(paste("./new_exp/imags/", tag, ".png", sep = ""), width = 1024, height = 768)
    plot(cbind(angle_seq,rs), type="l", main=tag, cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5, xlab = "angle(degree)", ylab = "objective")
    if(writeFile) dev.off()
  }
  max_lacc[,2] = 0
  filted_lacc = rotateHorizentalByAngle(max_lacc, -max_angle)
  return(list(max_angle = max_angle, filted_lacc = filted_lacc, max_target = max_target, records = rs))
}

targetFunc_v = function(rotated_acc, dt){
  v_arr_1 = integralAcc(rotated_acc[,1], dt)
  v_arr_2 = integralAcc(rotated_acc[,2], dt)
  return(abs( v_arr_1[length(v_arr_1)] ) / (abs( v_arr_2[length(v_arr_2)])+0.5) )
}

targetFunc_v_sd = function(rotated_acc, dt){
  v_arr_1 = integralAcc(rotated_acc[,1], dt)
  v_arr_2 = integralAcc(rotated_acc[,2], dt)
  sd_1 = sd(v_arr_1)
  sd_2 = sd(v_arr_2)
  return( 1.0/sd_2)
}

targetFunc_pos = function(rotated_acc, dt){
  v_arr_1 = integralAcc(integralAcc(rotated_acc[1:100, 1], dt), dt)
  v_arr_2 = integralAcc(integralAcc(rotated_acc[1:100, 2], dt), dt)
  return(abs( v_arr_1[length(v_arr_1)] ) / (abs( v_arr_2[length(v_arr_2)])+0.05) )
}
