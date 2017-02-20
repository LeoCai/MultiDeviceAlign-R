walk_top = read.csv("./new_exp/datas/performace/top.csv",header = F)
walk_lefthand = read.csv("./new_exp/datas/performace/Lefthand.csv",header = F)
walk_righthand = read.csv("./new_exp/datas/performace/Righthand.csv",header = F)
walk_leftpants = read.csv("./new_exp/datas/performace/Leftpants.csv",header = F)
walk_rightpants = read.csv("./new_exp/datas/performace/Rightpants.csv",header = F)

run_top = read.csv("./new_exp/datas/performace/Runtop.csv",header = F)
run_pants = read.csv("./new_exp/datas/performace/Runpants.csv",header = F)
run_hands = read.csv("./new_exp/datas/performace/Runlrhand.csv",header = F)

jump_pants = read.csv("./new_exp/datas/performace/Jumplrpants.csv",header = F)


displayPerformance = function(da, offset = 0){
  mean_da = sapply(da, mean)
  sd_da = sapply(da, sd)
  matplot(da[,],type="l", ylab = "degree", xlab = "expId")
  return(round(c(mean_da, sd_da), 2))
}


displayPerformance(walk_top)
displayPerformance(walk_lefthand)
displayPerformance(walk_righthand)
displayPerformance(walk_leftpants)
displayPerformance(walk_rightpants)

displayPerformance(run_top)
displayPerformance(90-run_hands)
displayPerformance(run_pants)

displayPerformance(90-jump_pants)

