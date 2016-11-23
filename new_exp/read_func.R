readDataSet = function(path, action, expNum){
  top_path = paste(path, "top/", action, expNum, ".csv", sep="")
  leftpants_path = paste(path, "leftpants/", action, expNum,".csv", sep="")
  rightpants_path = paste(path, "rightpants/", action, expNum, ".csv", sep="")
  top_data = read.csv(top_path)
  leftpants_data = read.csv(leftpants_path)
  rightpants_data = read.csv(rightpants_path)
  return(list(top_data, leftpants_data, rightpants_data))
}

