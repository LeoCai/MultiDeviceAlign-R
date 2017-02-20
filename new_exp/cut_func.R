cutByGryo = function(gyro_grivity, threhold = 3, off_ratio_start = 1, off_ratio_end = 2.1){
  plot(gyro_grivity, type = "l", main = "gyro_grivity")
  startIndex = 1; endIndex = 1
  cutIndexes = list(); cuI = 1;
  start = F;
  
  len_seq = c()
  
  for(i in 1:length(gyro_grivity)){
    gyro = gyro_grivity[i]
    if(!start){
      if( gyro > threhold){
        start = T; startIndex = i
      }
    }else{
      if( gyro < -threhold){
        start = F; endIndex = i
        len = endIndex - startIndex; len_seq = c(len_seq, len)
        cutIndexes[[cuI]] = c(startIndex - len/off_ratio_start, endIndex + len/off_ratio_end)
        cuI = cuI + 1;
      }
    }
  }
 
  
  #remove too long sequnce
  avg_len = mean(len_seq); sd_len = sd(len_seq)
  filted_indexes = list(); cuI = 1;
  for(i in 1:length(cutIndexes)){
    cutId = cutIndexes[[i]]; len = cutId[2] - cutId[1]
    if(abs(len - avg_len) < 2*sd_len){
      filted_indexes[[cuI]] = cutId; cuI  = cuI+1;
    }
  }
  
  for(i in 1:length(filted_indexes)){
    oneCut = filted_indexes[[i]]
    abline(v = oneCut[1], col=2,  lty = 2)
    abline(v = oneCut[2], col=3,  lty = 2)
  }
  
  return(filted_indexes)
}
