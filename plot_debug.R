library(heplots)
library(rgl)


# Fi = topForward[100:500,]
# Fi[,3] = 0
#
# FcVector = c(1,2,3)
# magVector = c(1,1,0)


debugPlot3dVector = function(Fi, FcVector,  magVector, tag="") {
  d = rbind(c(0,0,0))
  plot3d(d,xlim = -2:2,ylim = -2:2, zlim = -2:2, main = paste(tag,"3d vector"), xlab = "x", ylab = "y", zlab = "z")
  for (i in 1:nrow(Fi)) {
    if (absVector1(Fi[i,]) < 1)
      arrow3d(
        c(0,0,0),Fi[i,], barblen = .2, lwd = 3, col = "black"
      )
  }
  arrow3d(
    c(0,0,0),FcVector, barblen = .2, lwd = 3, col = "red"
  )
  arrow3d(
    c(0,0,0),magVector, barblen = .2, lwd = 3, col = "blue"
  )
}
# debugPlot3dVector(Fi, FcVector, magVector)

init3d = function(tag = ""){
  d = rbind(c(0,0,0))
  plot3d(d,xlim = -2:2,ylim = -2:2, zlim = -2:2, main = paste(tag,"3d vector"), xlab = "x", ylab = "y", zlab = "z",cex.main = 2,cex.lab=2)
}
myArrow3d = function(v,color = 1){
  arrow3d(
    c(0,0,0),v, barblen = .2, lwd = 3, col = color
  )
}
# v_body = c(2,3,5)
# gv = c(2,6,4)
# mag = c(1,3,4)
# globalRm = getMatrixByMag(gv, mag)
# v_global = rotationByMatrix(v_body,globalRm)
# v_global[3] = 0
# v_gb2 = rotationByMatrix(v_global,t(globalRm))
# init3d()
# myArrow3d(v_body,1)
# myArrow3d(gv,2)
# myArrow3d(mag,3)
# myArrow3d(v_global,4)
# # init3d()
# # myArrow3d(globalRm[,1],3)
# # myArrow3d(globalRm[,2],4)
# # myArrow3d(globalRm[,3],5)
# # myArrow3d(gv,1)
# # myArrow3d(mag,2)
# 
# init3d()
# myArrow3d(v_body,1)
# myArrow3d(v_gb2,3)
# myArrow3d(globalRm[,3],2)
# myArrow3d(globalRm[,2],3)
# 
# 
# 
# 
# rm = matrix(c(1,0,0,0,1,0,0,0,1),nrow = 3, ncol = 3)
# tmp = rm[,1];rm[,1] = rm[,2];rm[,2] =tmp 
# rotationByMatrix(v,rm)





