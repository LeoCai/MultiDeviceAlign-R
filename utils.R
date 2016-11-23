source("../handshake_9_11/model/GloableConvert.R")
library(zoo)

appendList = function(old_list, new_data){
  old_list[[length(old_list)+1]] = new_data
  return(old_list)
}

lowFilter = function(mdata,n = 3){
  # rollapply(mdata,n,mean)
  bf = butter(1, 1/8)
  b <- filter(bf, mdata)
  b[1:20] = mdata[1:20]
  return(b)
}

getInitMatrix = function(data) {
  lx = data$LinearAcc0; ly = data$LinearAcc1; lz = data$LinearAcc2;
  gx = data$Gravity0; gy = data$Gravity1; gz = data$Gravity2;
  linearAcc = c(lx,ly,lz); gravity = c(gx, gy, gz)
  gravity = unitVector(gravity)
  axis2 = product(linearAcc, gravity)
  axis2 = unitVector(axis2) #left right
  axis3 = product(axis2, gravity) #forward back
  axis3 = unitVector(axis3)
  initMatrix = matrix(
    c(
      axis2[1],axis2[2],axis2[3],
      axis3[1],axis3[2],axis3[3],
      gravity[1],gravity[2],gravity[3]
    ),
    nrow = 3,ncol = 3
  )
  return (initMatrix)
}

convertBaseOnFt = function(data) {
  gyrs = cbind(data$Gyro0, data$Gyro1, data$Gyro2)
  initMatrix = getInitMatrix(data[1,])
  rtms = updateMatrixByMultiGYR(gyrs, initMatrix, 0.02)
  accs = cbind(data$LinearAcc0, data$LinearAcc1, data$LinearAcc2)
  globalAccs = getGlobleAccs(accs, rtms)
  k = data.frame()
  for(i in 1:nrow(globalAccs)){
    k = rbind(k,unitVector(as.numeric(globalAccs[i,])))
  }
  return(k)
}

unitVector = function(vector) {
  absVector  = norm(matrix(vector),type = "2")
  if(absVector == 0) return (vector)
  return(vector / absVector)
}

readData = function(fileHead,fileTag,fileEnd, start, end) {
  file = paste(fileHead,fileTag,fileEnd, sep = "")
  data = read.csv(file,colClasses = rep("numeric"))
  data = data[start:end,]
  return(data)
}

absVector = function(v1, v2, v3) {
  return(sqrt(v1 ^ 2 + v2 ^ 2 + v3 ^ 2))
}

normVector = function(v1, v2, v3) {
  abs = absVector(v1, v2, v3)
  return(cbind(v1 / abs, v2 / abs, v3 / abs))
}

combineVectors = function (data) {
  return(sqrt(as.numeric(data[,1]) ^2 +as.numeric(data[,2]) ^2  +as.numeric(data[,3]) ^2 ))
}

filterGravity = function(ax, ay, az, len) {
  gravity = as.double(as.vector(c(ax[1],ay[1],az[1])))
  #gravity = as.double(as.vector(c(0,0,0)))
  alpha = 0.8
  
  gv1 = as.double(c(1:len))
  gv2 = as.double(c(1:len))
  gv3 = as.double(c(1:len))
  gvectors = data.frame(gv1, gv2, gv3)
  
  lv1 = as.double(c(1:len))
  lv2 = as.double(c(1:len))
  lv3 = as.double(c(1:len))
  lvectors = data.frame(lv1, lv2, lv3)
  
  for (i in 1:len) {
    gravity[1] = alpha * gravity[1] + (1 - alpha) * ax[i]
    gravity[2] = alpha * gravity[2] + (1 - alpha) * ay[i]
    gravity[3] = alpha * gravity[3] + (1 - alpha) * az[i]
    
    gvectors[i, 1] = gravity[1]
    gvectors[i, 2] = gravity[2]
    gvectors[i, 3] = gravity[3]
    
    lvectors[i, 1] = ax[i] - gravity[1]
    lvectors[i, 2] = ay[i] - gravity[2]
    lvectors[i, 3] = az[i] - gravity[3]
  }
  return (cbind(gvectors,  lvectors, combineVectors(gvectors)))
  
}

computeAngle = function(v1, v2){
  sum = 0
  for(i in 1:3){
    sum = sum + v1[i]*v2[i]
  }
  cosAngle = sum / (absVector(v1[1],v1[2],v1[3])*absVector(v2[1],v2[2],v2[3]))
  sign = 1
  
  return(sign*acos(cosAngle)*180/pi)
}

computeProjOnGravaty = function(lv, gv) {
  absProj = ((
    lv[,1] * gv[1] + lv[,2] * gv[2] + lv[,3] * gv[3]
  ) /
    absVector(gv[1], gv[2], gv[3])
  )
  
  nvg = gv/absVector(gv[1],gv[2],gv[3])
  results = data.frame()
  for(i in 1:length(absProj)){
    results = rbind(results,nvg*absProj[i])
  }
  return(results)
}

computeProjOnHori = function(lv, gv) {
  pg = computeProjOnGravaty(lv, gv)
  return(cbind(lv[,1] - pg[,1],lv[,2] - pg[,2],lv[,3] - pg[,3]))
}

computeProjOnHoriSingleData = function(lv, gv) {
  absProj = ((
    lv[1] * gv[1] + lv[2] * gv[2] + lv[3] * gv[3]
  ) /
    absVector(gv[1], gv[2], gv[3])
  )
  pg = gv/absVector(gv[1],gv[2],gv[3])*absProj
  return(cbind(lv[1] - pg[1], lv[2] - pg[2], lv[3] - pg[3]))
}

#PCA proces:
pcaProcess = function(data){
  epca6A <- prcomp(data,
                   center = TRUE,
                   scale. = FALSE)
  data = predict(epca6A, newdata = data)
  return(epca6A$rotation[,1])
}

cone3d <- function(base=c(0,0,0),tip=c(0,0,1),rad=1,n=30,draw.base=TRUE,qmesh=FALSE,
                   trans = par3d("userMatrix"), ...) {
  ax <- tip-base
  if (missing(trans) && !rgl.cur()) trans <- diag(4)
  ### is there a better way?
  if (ax[1]!=0) {
    p1 <- c(-ax[2]/ax[1],1,0)
    p1 <- p1/sqrt(sum(p1^2))
    if (p1[1]!=0) {
      p2 <- c(-p1[2]/p1[1],1,0)
      p2[3] <- -sum(p2*ax)
      p2 <- p2/sqrt(sum(p2^2))
    } else {
      p2 <- c(0,0,1)
    }
  } else if (ax[2]!=0) {
    p1 <- c(0,-ax[3]/ax[2],1)
    p1 <- p1/sqrt(sum(p1^2))
    if (p1[1]!=0) {
      p2 <- c(0,-p1[3]/p1[2],1)
      p2[3] <- -sum(p2*ax)
      p2 <- p2/sqrt(sum(p2^2))
    } else {
      p2 <- c(1,0,0)
    }
  } else {
    p1 <- c(0,1,0); p2 <- c(1,0,0)
  }
  degvec <- seq(0,2*pi,length=n+1)[-1]
  ecoord2 <- function(theta) {
    base+rad*(cos(theta)*p1+sin(theta)*p2)
  }
  i <- rbind(1:n,c(2:n,1),rep(n+1,n))
  v <- cbind(sapply(degvec,ecoord2),tip)
  if (qmesh) 
    ## minor kluge for quads -- draw tip twice
    i <- rbind(i,rep(n+1,n))
  if (draw.base) {
    v <- cbind(v,base)
    i.x <- rbind(c(2:n,1),1:n,rep(n+2,n))
    if (qmesh)  ## add base twice
      i.x <-  rbind(i.x,rep(n+2,n))
    i <- cbind(i,i.x)
  }
  if (qmesh) v <- rbind(v,rep(1,ncol(v))) ## homogeneous
  if (!qmesh)
    triangles3d(v[1,i],v[2,i],v[3,i],...)
  else
    return(rotate3d(qmesh3d(v,i,material=...), matrix=trans))
}     

arrows3d = function(v,lim3d = c(-1,1),title="3d"){
  open3d()
  data = rbind(c(0,0,0),v)
  plot3d(data,type = "l", col="red", lwd = 5, xlim = lim3d, ylim = lim3d, zlim = lim3d,main  =title, cex.main = 1.5, cex.lab = 1.5,xlab = "right",ylab="forward",zlab="up")
#   vec=rbind( c( 0, 0, 0 ), v )
#   segments3d( vec )
#   cone3d(base=vec[2,]-(vec[1,]+vec[2,]/6), 
#          #this makes the head go 1/6th the length of the arrow
#          rad=0.1,
#          tip=vec[2,],
#          col="blue",
#          front="lines",
#          back="lines")
}

crossProduct = function(a, b){
  c = rep(0,3)
  c[1] = a[2]*b[3] - a[3]*b[2]
  c[2] = a[3]*b[1] - a[1]*b[3]
  c[3] = a[1]*b[2] - a[2]*b[1]
  return(c)
}

dotProduct = function(a,b){
  return(a[1]*b[1]+a[2]*b[2]+a[3]*b[3])
}

absVector1 = function(v){
  return(sqrt(v[1]^2 + v[2]^2 + v[3]^2))
}

vectorToMatrix = function(v){
  return(matrix(v,nrow=3))
}

matrixToVector = function(matrixV){
  return (c(matrixV[1,1],matrixV[2,1],matrixV[3,1]))
}

rotationByMatrix = function(v, rotationMatrix){
  temp = rotationMatrix%*%vectorToMatrix(v)
  return(matrixToVector(temp))
}

computeFi = function(myData) {
  firstData = myData[1,]
  gvInit = cbind(firstData$Gravity0,firstData$Gravity1,firstData$Gravity2)
  mgInit = cbind(firstData$MagnetData0,firstData$MagnetData1,firstData$MagnetData2)
  initRotationMatirx = getMatrixByMag(gvInit,mgInit)
  globalAccs = getGlobalAccByMag(myData)
  globalAccs[,3] = 0; #proj on horizontal
  matplot(globalAccs,type = "l",col = c(1,2,3),main = "global accs")
  rotatedGlobal = c()
  for (i in 1:nrow(globalAccs)) {
    temp = initRotationMatirx %*% matrix(globalAccs[i,],nrow = 3,byrow = F)
    rotatedAcc = c(temp[1,1],temp[2,1],temp[3,1])
    # rotatedAccOnHori = computeProjOnHoriSingleData(rotatedAcc,gvInit)
    rotatedGlobal = rbind(rotatedGlobal, rotatedAcc)
  }
  return(list(rotatedGlobal,initRotationMatirx))
}

getDirection = function(v1,v2, gv){
  tempV = crossProduct(v1,v2)
  angle = computeAngle(tempV,gv)
  if(angle>90) return(-1)
  else return(1)
}

sampleByData = function(mdata, step = 2){
  end = nrow(mdata)
  return(mdata[seq(1,end,step),])
}
