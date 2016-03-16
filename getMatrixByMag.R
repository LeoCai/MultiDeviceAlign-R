getMatrixByMag = function(gv,mag){
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

# d = top
# 
# 
# plot(globalAcc[,1],type = "l", main="1")
# plot(globalAcc[,2],type = "l", main="2")
# library(zoo)
# matplot(cbind(globalAcc[,2],d$ConvertedData1),type="l")
# plot(globalAcc[,3],type = "l", main="3")

getGlobalAccByMag = function(d){
  laccs = cbind(d$LinearAcc0,d$LinearAcc1,d$LinearAcc2)
  gaccs = cbind(d$Gravity0, d$Gravity1, d$Gravity2)
  mags  = cbind(d$MagnetData0, d$MagnetData1, d$MagnetData2)
  globalAcc = c()
  for(i in 1:nrow(d)){
    lacc = laccs[i,]
    gv   = gaccs[i,]
    mag  = mags[i,]
    rotationMatrix = getMatrixByMag(gv, mag)
    tempGV = t(rotationMatrix)%*%matrix(lacc,nrow=3,byrow=F)
    globalAcc = rbind(globalAcc,c(tempGV[1,1],tempGV[2,1],tempGV[3,1]))
  }
  return(globalAcc)
}

# globalAcc = getGlobalAccByMag(top)
# plot(globalAcc[,3],type = "l", main="3")
