require(akima) 
require(rgl)

magFloor = read.csv("./datas/2_26_mag_test/MagTestIndoorFloor.csv",header = F)
mag427 = read.csv("./datas/2_26_mag_test/MagTestIndoor427.csv",header = F)

plotMag = function(mdata){
  maxRow = max(mdata[,1])
  maxCol = max(mdata[,2])
  z = matrix(nrow = maxRow,ncol = maxCol)
  for(i in 1:nrow(mdata)){
    z[mdata[i,1],mdata[i,2]] = mdata[i,3]
  }
  persp( z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
}

plotMag(magFloor)


# x <- seq(-10, 10, length= 30)
# y <- x
# f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
# z <- outer(x, y, f)
# z[is.na(z)] <- 1
# op <- par(bg = "white")
# persp( z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")