trackData = function(gaccs, gyrs, dts, laccs, init_x){
  gacc_init = gaccs[1,]; global_x = init_x
  rmt_g2b = getRotationMatrixG2BBy2Vectors(gacc_init, global_x)
  global_x = rmt_g2b[,2]
  new_laccs = matrix(nrow  = nrow(laccs), ncol = 3)
  
  for(i in 1:nrow(laccs)){
    gyr = as.vector(gyrs[i,]); grivity = as.vector(gaccs[i,]); dt = as.vector(dts[i]); lacc = as.vector(laccs[i,])
    w_grivity = as.numeric(project(gyr, grivity))
    rtm_b2g = quaternionToMatrix(w_grivity * dt, grivity)
    next_global_mat_x = t(rtm_b2g)%*% as.matrix(as.numeric(global_x))
    global_x = as.vector(next_global_mat_x)
    rmt_g2b = getRotationMatrixG2BBy2Vectors(grivity, global_x)
    global_x = rmt_g2b[,2]
    new_lacc = getGlobalData(lacc, rmt_g2b);
    new_laccs[i,] = new_lacc
  }
  
  return(new_laccs)
}

getGlobalData = function(lacc, rtm_g2b){
  return(as.vector((t(rtm_g2b)%*%as.matrix(as.numeric(lacc)))))
}

getRotationMatrixG2BBy2Vectors = function(gv,mag){
  gv = as.numeric(gv)
  mag = as.numeric(mag)
  #wait for test
  Ax = gv[1]; Ay = gv[2]; Az = gv[3]
  Ex = mag[1]; Ey = mag[2]; Ez = mag[3]
  Hx = Ey*Az - Ez*Ay; Hy = Ez*Ax - Ex*Az; Hz = Ex*Ay - Ey*Ax
  normH = sqrt(Hx*Hx + Hy*Hy + Hz*Hz)
  invH = 1.0 / normH
  Hx = Hx*invH; Hy = Hy*invH; Hz = Hz*invH
  invA = 1.0 / sqrt(Ax*Ax + Ay*Ay + Az*Az)
  Ax = Ax*invA; Ay = Ay*invA; Az = Az*invA
  Mx = Ay*Hz - Az*Hy; My = Az*Hx - Ax*Hz; Mz = Ax*Hy - Ay*Hx
  return (matrix(data= c(Hx,Hy,Hz,Mx,My,Mz,Ax,Ay,Az), nrow=3, ncol = 3, byrow = F))
}

dotMultiply = function(v1, v2){
  return(v1[1]*v2[1]+v1[2]*v2[2]+v1[3]*v2[3])
}

absVector = function(v){
  return(sqrt(v[1]^2+v[2]^2+v[3]^2))
}

project = function(vector, direction){
  return (dotMultiply(vector, direction) / absVector(direction))
}

quaternionToMatrix = function(angle, u){
  norm = absVector(u)
  u[1] = u[1] / norm; u[2] = u[2] / norm; u[3] = u[3] / norm 
  rotationMatrix = matrix(nrow = 3, ncol = 3)
  rotationMatrix[1,1] = as.numeric(cos(angle) + u[1]^2*(1-cos(angle)))
  rotationMatrix[1,2] = as.numeric(u[1]*u[2]*(1-cos(angle)) - u[3]*sin(angle))
  rotationMatrix[1,3] = as.numeric(u[2]*sin(angle) + u[1]*u[3]*(1-cos(angle)))
  
  rotationMatrix[2,1] = as.numeric(u[3]*sin(angle) + u[1]*u[2]*(1-cos(angle)))
  rotationMatrix[2,2] = as.numeric(cos(angle) + u[2]^2 * (1-cos(angle)))
  rotationMatrix[2,3] = as.numeric(-u[1]*sin(angle) + u[2]*u[3]*(1-cos(angle)))
  
  rotationMatrix[3,1] = as.numeric(-u[2]*sin(angle) + u[1]*u[3]*(1-cos(angle)))
  rotationMatrix[3,2] = as.numeric(u[1]*sin(angle) + u[2]*u[3]*(1-cos(angle)))
  rotationMatrix[3,3] = as.numeric(cos(angle) + u[3]^2*(1-cos(angle)))
  
  return(rotationMatrix)
}