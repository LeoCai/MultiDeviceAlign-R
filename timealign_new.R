glass_data = read.csv("./datas/3_1_data/glass/11.csv")
top_data = read.csv("./datas/3_1_data/top/11.csv")
leftPants_data = read.csv("./datas/3_1_data/leftpants/11.csv")
rightPants_data = read.csv("./datas/3_1_data/rightpants/11.csv")
leftHand_data = read.csv("./datas/3_1_data/lefthand/11.csv")
rightHand_data = read.csv("./datas/3_1_data/righthand/11.csv")

testAlign = function(data1, data2,s1,e1,tag="") {
  c1 = sqrt(data1$Acc0 ^ 2 + data1$Acc1 ^ 2 + data1$Acc2 ^ 2)
  c2 = sqrt(data2$Acc0 ^ 2 + data2$Acc1 ^ 2 + data2$Acc2 ^ 2)
  c11 = c1[s1:e1]
  maxCor = 0
  maxS2 = s1
  maxE2 = e1
  range = 20
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
  matplot(1:length(c11),cbind(c11,c22),type = "l",main = paste(tag,round(maxCor, 4)))
  print(paste(tag,"[",maxS2,":",maxE2,",]", sep=""))
  print(round(maxCor, 4))
  print("")
  return (c(s1,e1,maxS2,maxE2,maxS2 - s1))
  # matplot(1:length(c1),cbind(c1,c2),type="l",main = round(cor(c1,c2), 4) )
}



for (p in 2) {
  for(n in 1:3){

    
    print(paste("exp",p,n,"start----------------------"))
    glass_data = read.csv(paste("./datas/3_1_data/glass/",p,n,".csv",sep = "")); glass_data = sampleByData(glass_data)
    leftHand_data = read.csv(paste("./datas/3_1_data/lefthand/",p,n,".csv",sep = "")); leftHand_data = sampleByData(leftHand_data)
    rightHand_data = read.csv(paste("./datas/3_1_data/righthand/",p,n,".csv",sep = "")); rightHand_data = sampleByData(rightHand_data)
    leftPants_data = read.csv(paste("./datas/3_1_data/leftpants/",p,n,".csv",sep = "")); leftPants_data = sampleByData(leftPants_data)
    rightPants_data = read.csv(paste("./datas/3_1_data/rightpants/",p,n,".csv",sep = "")); rightPants_data = sampleByData(rightPants_data)
    top_data = read.csv(paste("./datas/3_1_data/top/",p,n,".csv",sep = "")); top_data = sampleByData(top_data)
    s = 100; e = 800
    # print("top_data")
    r5=testAlign(top_data, top_data,s,e,"top")
    # print(r5)
    # print("")
    
    # print("leftHand_data")
    r1 = testAlign(top_data, leftHand_data,s,e,"lefthand")
    # print(r1)
    # print("")
    
    # print("rightHand_data")
    r2= testAlign(top_data, rightHand_data,s,e,"righthand")
    # print(r2)
    # print("")
    
    # print("leftPants_data")
    r4=testAlign(top_data, leftPants_data,s,e,"leftpants")
    # print(r4)
    # print("")
    
    # print("rightPants_data")
    r3 = testAlign(top_data, rightPants_data,s,e,"rightpants")
    # print(r3)
    # print("")
    
    # print("glass_data")
    r6=testAlign(top_data, glass_data,s,e,"glass")
    # print(r6)
    # print("")
    
  }
}