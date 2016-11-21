library(zoo)
source("./new_exp/test_read.R")
source("./utils.R")
source("./getMatrixByMag.R")

testAlign = function(data1, data2,s1,e1,tag = "") {
  c1 = sqrt(data1$LinearAcc0 ^ 2 + data1$LinearAcc1 ^ 2 + data1$LinearAcc2 ^ 2)
  c2 = sqrt(data2$LinearAcc0 ^ 2 + data2$LinearAcc1 ^ 2 + data2$LinearAcc2 ^ 2)
  data1_selected = c1[s1:e1]
  maxCor = 0; best_start2 = s1; best_end2 = e1; range = round(s1 / 2)
  for (i in 1:range) {
    s2 = s1 - range / 2 + i; e2 = e1 - range / 2 + i
    data2_selected = c2[s2:e2]
    cvdata = cor(data1_selected,data2_selected)
    if (cvdata > maxCor) { best_start2 = s2; best_end2 = e2; maxCor = cvdata}
  }
  data2_selected = c2[best_start2:best_end2]
  matplot(1:length(data1_selected),cbind(data1_selected, data2_selected),type = "l",main = paste(tag,round(maxCor, 4)))
  return (best_start2:best_end2)
}

aligned_data_set = alignData(list(walk_top, walk_legr, walk_legl), start = 400, end = 800)
resultantPlot(aligned_data_set, "WALK_ALINED_RESULTANT_ACCEROMETER",c(1:150),T)

aligned_data_set = alignData(list(run_top, run_legr, run_legl), start = 100, end = 300)
resultantPlot(aligned_data_set, "RUN_ALINED_RESULTANT_ACCEROMETER",c(1:100),T)

aligned_data_set = alignData(list(forjump_top, forjump_legr, forjump_legl), start = 300, end = 800)
resultantPlot(aligned_data_set, "FORJUMP_ALINED_RESULTANT_ACCEROMETER",c(1:150),T)
