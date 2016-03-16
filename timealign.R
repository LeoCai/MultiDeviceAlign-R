
#sync time 
glass_data = read.csv("~/Documents/mutidevicealign/1_29/Glass_1.csv")
leftHand_data = read.csv("~/Documents/mutidevicealign/1_29/LeftHand_1.csv")
rightHand_data = read.csv("~/Documents/mutidevicealign/1_29/RightHand_1.csv")
leftPants_data = read.csv("~/Documents/mutidevicealign/1_29/LeftPants_1.csv")
rightPants_data = read.csv("~/Documents/mutidevicealign/1_29/RightPants_1.csv")
top_data = read.csv("~/Documents/mutidevicealign/1_29/Top_1.csv")
head(glass_data)

testAlign = function(data1, data2,s1,e1) {
  c1 = sqrt(data1$Acc0 ^ 2 + data1$Acc1 ^ 2 + data1$Acc2 ^ 2)
  c2 = sqrt(data2$Acc0 ^ 2 + data2$Acc1 ^ 2 + data2$Acc2 ^ 2)
  c11 = c1[s1:e1]
  maxCor = 0
  maxS2 = s1
  maxE2 = e1
  range = round(s1/2)
  for (i in 1:range) {
    s2 = s1 - range/2 + i
    e2 = e1 - range/2 + i
    c22 = c2[s2:e2]
    cvdata = cor(c11,c22)
    # print(paste(s2,e2,cvdata))
    if (cvdata > maxCor) {
      maxS2 = s2; maxE2 = e2
      maxCor = cvdata
    }
    # print(cvdata)
  }
  c22 = c2[maxS2:maxE2]
  matplot(1:length(c11),cbind(c11,c22),type = "l",main = round(maxCor, 4))
  return (c(s1,e1,maxS2,maxE2,maxS2 - s1))
  # matplot(1:length(c1),cbind(c1,c2),type="l",main = round(cor(c1,c2), 4) )
}

testAlign(rightHand_data, leftHand_data,100,300)
testAlign(rightHand_data, leftPants_data,100,300)
testAlign(rightHand_data, rightPants_data,100,300)
testAlign(rightHand_data, leftPants_data,100,300)
testAlign(rightHand_data, top_data,100,300)

for (i in 2) {
  print(paste("exp",i,"start----------------------"))
  glass_data = read.csv(paste("~/Documents/mutidevicealign/1_29/Glass_",i,".csv",sep = ""))
  for(j in 1:3){
    glass_data[,j] = glass_data[,j]*9.8
  }
  leftHand_data = read.csv(paste("~/Documents/mutidevicealign/1_29/LeftHand_",i,".csv",sep = ""))
  rightHand_data = read.csv(paste("~/Documents/mutidevicealign/1_29/RightHand_",i,".csv",sep = ""))
  leftPants_data = read.csv(paste("~/Documents/mutidevicealign/1_29/LeftPants_",i,".csv",sep = ""))
  rightPants_data = read.csv(paste("~/Documents/mutidevicealign/1_29/RightPants_",i,".csv",sep = ""))
  top_data = read.csv(paste("~/Documents/mutidevicealign/1_29/Top_",i,".csv",sep = ""))
  print("leftHand_data")
  r1 = testAlign(rightHand_data, leftHand_data,100,800)
  print(r1)
  print("")
  
  
  print("rightHand_data")
  r2= testAlign(rightHand_data, leftPants_data,100,800)
  print(r2)
  print("")
  
  print("rightPants_data")
  r3 = testAlign(rightHand_data, rightPants_data,100,800)
  print(r3)
  print("")
  
  print("leftPants_data")
  r4=testAlign(rightHand_data, leftPants_data,100,800)
  print(r4)
  print("")
  
  print("top_data")
  r5=testAlign(rightHand_data, top_data,100,800)
  print(r5)
  print("")
  
  print("glass_data")
  rightHand_data_10F = rightHand_data[seq(1,800,5),]
  r6=testAlign(rightHand_data, glass_data,20,200)
  print(r6)
  print("")
  
}
