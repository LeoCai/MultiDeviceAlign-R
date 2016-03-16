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
  matplot(1:length(c11),cbind(c11,c22),type = "l",main = paste(tag,round(maxCor, 4)))
  print(paste(tag,"[",maxS2,":",maxE2,",]", sep = ""))
  print(maxCor)
  print("")
  return (c(maxS2,maxE2))
  # matplot(1:length(c1),cbind(c1,c2),type="l",main = round(cor(c1,c2), 4) )
}

mylowpass = function(top) {
  return(as.data.frame(rollapply(top,3,mean)))
}

getUp = function(mdata,tag="",isUp = T){
  s1 = 100;e1 = 700
  s2 = nrow(mdata) - 200; e2 = nrow(mdata)
  dataUp = mdata[s1:e1,]
  dataDown = mdata[s2:e2,]
  return(dataUp)
}

i = 2

top_data = read.csv(paste("./datas/3_3_data_speedup_down/top/1",i,".csv",sep = ""))
leftHand_data = read.csv(paste("./datas/3_3_data_speedup_down/lefthand/1",i,".csv",sep = ""))
rightHand_data = read.csv(paste("./datas/3_3_data_speedup_down/righthand/1",i,".csv",sep = ""))
leftPants_data = read.csv(paste("./datas/3_3_data_speedup_down/leftpants/1",i,".csv",sep = ""))
rightPants_data = read.csv(paste("./datas/3_3_data_speedup_down/rightpants/1",i,".csv",sep = ""))
glass_data = read.csv(paste("./datas/3_3_data_speedup_down/glass/1",i,".csv",sep = ""))


top_data = getUp(top_data)
leftHand_data = getUp(leftHand_data)
rightHand_data = getUp(rightHand_data)
leftPants_data = getUp(leftPants_data)
rightPants_data = getUp(rightPants_data)
glass_data = getUp(glass_data)

# mdata = top;tag = "top"

# plot(gup[,2],type = "l",main = paste(tag,"up"))
# plot(gdown[,2],type = "l",main = paste(tag,"down"))

start = 100; end = 500
r = testAlign(top_data, top_data,start,end,"top")
top = top_data[r[1]:r[2],]

r = testAlign(top_data, leftHand_data,start,end,"lefthand")
leftHand = leftHand_data[r[1]:r[2],]

r = testAlign(top_data, rightHand_data,start,end,"righthand")
rightHand = rightHand_data[r[1]:r[2],]

r = testAlign(top_data, leftPants_data,start,end,"leftpants")
leftPants = leftPants_data[r[1]:r[2],]

r = testAlign(top_data, rightPants_data,start,end,"rightpants")
rightPants = rightPants_data[r[1]:r[2],]

r = testAlign(top_data, glass_data,start,end,"glass")
glass = glass_data[r[1]:r[2],]

top = mylowpass(top)
leftHand = mylowpass(leftHand)
rightHand = mylowpass(rightHand)
leftPants = mylowpass(leftPants)
rightPants = mylowpass(rightPants)
glass = mylowpass(glass)
