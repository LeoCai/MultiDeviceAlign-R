pcaprocess = function(expTag,ld1, ld2, ld3,XYZTYPE = T){
  if(XYZTYPE)
    axis = c("X","Y","Z")
  else axis = c("LEFT_RIGHT","FORWARD","GRAVITY")
  for(i in 1:3){
    data = cbind(ld1[,i],ld2[,i],ld3[,i])
    epca6A <- prcomp(data,
                     center = TRUE,
                     scale. = FALSE)
    data = predict(epca6A, newdata = data)
    if (writePlot)
      png(
        filename = paste("./imgs/1_18/",index,expTag," PCA_", axis[i],".png",sep = ""), width = 1360, height = 768
      )
    index <<- index+1
    par(mfrow = c(3,1) )
    for(j in 1:3){
      plot(data[,j], type = "l", ltw= 1.5, xlab = "index", ylab = "magnitude", cex.lab=1.5, cex.main = 1.5,  main = paste(expTag," PCA_", axis[i], " PC",j,sep = ""))
    }
    if (writePlot)
      dev.off()
  }
}

multiDecvicePCA = function(multiDeviceData){
  for(i in 1:4){
    for(deviceIndex in 1:length(multiDeviceData)){
      
    }
  }
  
}
