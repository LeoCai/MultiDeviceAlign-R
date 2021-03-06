#重要:陀螺仪更新相关函数

getUpdateMatrix = function(cuMatrix, gyr, dt) {
#利用前一时刻的旋转矩阵＋当前陀螺仪数据＋短暂时间，陀螺仪数据获得瞬时的更新矩阵
  #wait for test
  delta = sqrt((gyr[1] * dt) ^ 2 + (gyr[2] * dt) ^ 2 + (gyr[3] * dt) ^ 2)
  B = matrix(
    c(
      0,             -gyr[3] * dt,    gyr[2] * dt,
      gyr[3] * dt,   0,               -gyr[1] * dt,
      -gyr[2] * dt,  gyr[1] * dt,     0
    ),
    nrow = 3, ncol = 3, byrow = TRUE
  )
  B1 = B * (sin(delta) / delta)
  B2 = (1 - cos(delta)) / (delta ^ 2) * (B %*% B)
  updateMatrix = I + B1 + B2
  return(cuMatrix%*%updateMatrix)
}

RotationMatrix = function(angle, u){
#四元数获得旋转矩阵
  norm = absVector1(u)
  u[1] = u[1] / norm; u[2] = u[2] / norm; u[3] = u[3] / norm 
  rotationMatrix = matrix(nrow = 3, ncol = 3)
  rotationMatrix[1,1] = cos(angle) + u[1]^2*(1-cos(angle))
  rotationMatrix[1,2] = u[1]*u[2]*(1-cos(angle)) - u[3]*sin(angle)
  rotationMatrix[1,3] = u[2]*sin(angle) + u[1]*u[3]*(1-cos(angle))
  
  rotationMatrix[2,1] = u[3]*sin(angle) + u[1]*u[2]*(1-cos(angle))
  rotationMatrix[2,2] = cos(angle) + u[2]^2 * (1-cos(angle))
  rotationMatrix[2,3] = -u[1]*sin(angle) + u[2]*u[3]*(1-cos(angle))
  
  rotationMatrix[3,1] = -u[2]*sin(angle) + u[1]*u[3]*(1-cos(angle))
  rotationMatrix[3,2] = u[1]*sin(angle) + u[2]*u[3]*(1-cos(angle))
  rotationMatrix[3,3] = cos(angle) + u[3]^2*(1-cos(angle))
  
  return(rotationMatrix)
}

getCarlibratedMatirx = function(computedInitGV, initRealGV){
#利用计算出来的重力向量和实际重力向量算出补偿矩阵
  vectorBefore = computedInitGV; vectorAfter = initRealGV
  rotationAxis = crossProduct(vectorBefore, vectorAfter)
  rotationAngle = acos(dotProduct(vectorBefore, vectorAfter) / absVector1(vectorBefore)/absVector1(vectorAfter))
  # rotationAngles <<- c(rotationAngles, rotationAngle/pi*180)
  # print(paste("rotationAngle",rotationAngle/pi*180))
  rotationMatrix = RotationMatrix(rotationAngle, rotationAxis)
  return(rotationMatrix)
}

carlibrate = function(rm2,rotationAxis, gyromatrix){
#计算最优的校准
  mindist = 10000000;  bestAngle = 0;  bestRm = 0
  
  for(angle in 0:360){
    rm = RotationMatrix(angle/pi*180, rotationAxis);dist = mean((rm2%*%rm - gyromatrix)^2)
    if(dist<mindist){mindist = dist; bestAngle = angle;bestRm = rm2%*%rm}
    # print(paste(dist,angle))
  }
  # print(paste(mindist,bestAngle))
  return (bestRm)
}
