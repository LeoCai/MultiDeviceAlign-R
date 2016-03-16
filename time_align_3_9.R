testAlign = function(data1, data2,s1,e1,tag = "") {
  c1 = sqrt(data1$Acc0 ^ 2 + data1$Acc1 ^ 2 + data1$Acc2 ^ 2)
  c2 = sqrt(data2$Acc0 ^ 2 + data2$Acc1 ^ 2 + data2$Acc2 ^ 2)
  c11 = c1[s1:e1]
  maxCor = 0
  maxS2 = s1
  maxE2 = e1
  range = round(s1 / 2)
  for (i in 1:range) {
    s2 = s1 - range / 2 + i
    e2 = e1 - range / 2 + i
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
  # matplot(1:length(c11),cbind(c11,c22),type = "l",main = paste(tag,round(maxCor, 4)))
  # print(paste(tag,"[",maxS2,":",maxE2,",]", sep = ""))
  # print(round(maxCor, 4))
  # print("")
  return (c(maxS2,maxE2))
  # matplot(1:length(c1),cbind(c1,c2),type="l",main = round(cor(c1,c2), 4) )
}

mylowpass = function(top) {
  return(as.data.frame(rollapply(top,3,mean)))
}

path = 2
i = 3
s = 100
e = 3000
fileParent = "3_9_data_3path"

readSample = function(path,i,s,e){
  top_data = read.csv(paste("./datas/",fileParent,"/top/",path,i,".csv",sep = ""))
  leftHand_data = read.csv(paste("./datas/",fileParent,"/lefthand/",path,i,".csv",sep = ""))
  rightHand_data = read.csv(paste("./datas/",fileParent,"/righthand/",path,i,".csv",sep = ""))
  leftPants_data = read.csv(paste("./datas/",fileParent,"/leftpants/",path,i,".csv",sep = ""))
  rightPants_data = read.csv(paste("./datas/",fileParent,"/rightpants/",path,i,".csv",sep = ""))
  glass_data = read.csv(paste("./datas/",fileParent,"/glass/",path,i,".csv",sep = ""))
  
  r = testAlign(top_data, top_data,s,e,"top")
  top <<- top_data[r[1]:r[2],]
  
  r = testAlign(top_data, leftHand_data,s,e,"lefthand")
  leftHand <<- leftHand_data[r[1]:r[2],]
  
  r = testAlign(top_data, rightHand_data,s,e,"righthand")
  rightHand <<- rightHand_data[r[1]:r[2],]
  
  r = testAlign(top_data, leftPants_data,s,e,"leftpants")
  leftPants <<- leftPants_data[r[1]:r[2],]
  
  r = testAlign(top_data, rightPants_data,s,e,"rightpants")
  rightPants <<- rightPants_data[r[1]:r[2],]
  
  r = testAlign(top_data, glass_data,s,e,"glass")
  glass <<- glass_data[r[1]:r[2],]
  
  top <<- mylowpass(top)
  leftHand <<- mylowpass(leftHand)
  rightHand <<- mylowpass(rightHand)
  leftPants <<- mylowpass(leftPants)
  rightPants <<- mylowpass(rightPants)
  glass <<- mylowpass(glass)
}


