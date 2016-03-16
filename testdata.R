source("./utils.R")
source("./readData.R")

drawforward = function(mdata,tag=""){
  gacc = getGlobalAccByMag(mdata[1:500,])
  plot(gacc[,2],type="l",main = tag)
}

toppre = top
lefthandpre = leftHand
righthandpre = rightHand
leftpantspre = leftPants
rightpantspre = rightPants

source("./readData_new_1.R")

par(mfrow  = c(2,1))
drawforward(toppre,"toppre")
drawforward(top,"top")

drawforward(lefthandpre,"lefthandpre")
drawforward(leftHand,"leftHand")

drawforward(righthandpre,"righthandpre")
drawforward(rightHand,"rightHand")

drawforward(leftpantspre,"leftpantspre")
drawforward(leftPants,"leftPants")

drawforward(rightpantspre,"rightpantspre")
drawforward(rightPants,"rightPants")
