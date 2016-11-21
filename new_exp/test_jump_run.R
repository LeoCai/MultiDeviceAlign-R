library(zoo)
run_legl = read.csv("./new_exp/datas/leg_l/run.csv")
forjump_legl = read.csv("./new_exp/datas/leg_l/forjump.csv")
upjump_legl = read.csv("./new_exp/datas/leg_l/upjump.csv")
walk_legl = read.csv("./new_exp/datas/leg_l/walk.csv")

run_legr = read.csv("./new_exp/datas/leg_r/run.csv")
forjump_legr = read.csv("./new_exp/datas/leg_r/forjump.csv")
upjump_legr = read.csv("./new_exp/datas/leg_r/upjump.csv")
walk_legr = read.csv("./new_exp/datas/leg_r/walk.csv")

run_top = read.csv("./new_exp/datas/top/run.csv")
forjump_top = read.csv("./new_exp/datas/top/forjump.csv")
upjump_top = read.csv("./new_exp/datas/top/upjump.csv")
walk_top = read.csv("./new_exp/datas/top/walk.csv")

upjump2 = read.csv("./new_exp/Upjump2.csv")
forjump2 = read.csv("./new_exp/forjump.csv")

run_top_ = run_top[10:110,]
run_top = run_top[1:100,]
run_top = run_top[1:100,]
run_top = run_top[1:100,]

cp_action = cbind(run_top$ResultantAcc[10:110],walk_top$ResultantAcc[10:110],upjump_top$ResultantAcc[10:110],forjump_top$ResultantAcc[10:110])

matplot(cp_action,type="l",main = "different action on top")
legend("topright",legend = c("run","walk","upjump","fojump"),lty = 1:4,col = 1:4)

cp_up_jump = upjump_top[10:100,c("ConvertedData0","ConvertedData1","ConvertedData2")]
matplot(cp_up_jump,type="l",main = "upjump_top")
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3)


par(mfrow = c(2,1))
cp_for_jump = -forjump_top[1:200,c("ConvertedData0","ConvertedData1","ConvertedData2")]
matplot(cp_for_jump,type="l",main = "forjump_top")
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3)

cp_for_jump = forjump_top[1:400,c("LinearAcc0","LinearAcc1","LinearAcc2")]
matplot(cp_for_jump,type="l",main = "forjump_top_linear")
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3)

cp_up_jump = upjump_top[1:400,c("LinearAcc0","LinearAcc1","LinearAcc2")]
matplot(cp_up_jump,type="l",main = "upjump_top_linear")
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3)

up_jump_tmp = rollapply(upjump2,5,mean)
up_jump_tmp_l = up_jump_tmp[,c("LinearAcc0","LinearAcc1","LinearAcc2")]
matplot(up_jump_tmp_l,type="l",main = "upjump_linear_top",lwd = 2,cex = 2)
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3,cex = 2)

up_jump_tmp_g = getGlobalAccByMag(upjump2)
up_jump_tmp_g = rollapply(up_jump_tmp_g,5,mean)
matplot(up_jump_tmp_g,type="l",main = "upjump_global_top",lwd = 2,cex = 2)
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3,cex = 2)

for_jump_tmp = rollapply(for_jump2,5,mean)
for_jump_tmp = for_jump_tmp[,c("LinearAcc0","LinearAcc1","LinearAcc2")]
matplot(for_jump_tmp,type="l",main = "forjump_top", lty=c(3,2,1),col = c(3,2,1), lwd = 2,cex = 2)
legend("topright",legend = c("x","y","z"),lty=c(3,2,1),col = c(3,2,1),cex = 2)

for_jump_tmp_g = getGlobalAccByMag(forjump2)
for_jump_tmp_g = rollapply(for_jump_tmp_g,5,mean)
matplot(for_jump_tmp_g,type="l",main = "forjump_global_top",lwd = 2,cex = 2)
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3,cex = 2)

localtion = function(upjump2){
  x = upjump2$LinearAcc0*(upjump2$dt)
  y = upjump2$LinearAcc1*(upjump2$dt)
  z = upjump2$LinearAcc2*(upjump2$dt)
  loc = data.frame(x,y,z)
  for(i in 2:nrow(loc)){
    loc[i,] = loc[i-1,]+loc[i,]
  }
  loc[1,] = loc[1,]*upjump2$dt[1]
  for(i in 2:nrow(loc)){
    loc[i,] = loc[i-1,]*upjump2$dt[i-1]+loc[i,]
  }
  return(loc)
}

loc = localtion(upjump2)
matplot(loc,type = "l", main = "location")
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3,cex = 2)

loc = localtion(forjump2)
matplot(loc,type = "l", main = "location")
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3,cex = 2)


#PCA
up_jump_tmp_g = getGlobalAccByMag(upjump2)
up_jump_tmp_g = up_jump_tmp_g[8:nrow(up_jump_tmp_g),1:2]
pca_model = prcomp(up_jump_tmp_g,center = F,scale. = F)
plot(pca_model)
biplot(pca_model)
par(mfrow = c(2,1))
pca_up_jump = predict(pca_model, newdata = up_jump_tmp_g)
matplot(pca_up_jump,type = "l", main = "pca_location")
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3,cex = 2)
matplot(up_jump_tmp_g,type = "l", main = "linear_location")
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3,cex = 2)

for_jump_tmp_g = getGlobalAccByMag(forjump2)
for_jump_tmp_g = for_jump_tmp_g[8:nrow(for_jump_tmp_g),1:2]
pca_model = prcomp(for_jump_tmp_g,center = F,scale. = F)
plot(pca_model)
biplot(pca_model)
par(mfrow = c(2,1))
pca_for_jump = predict(pca_model, newdata = for_jump_tmp_g)
matplot(cbind(pca_for_jump,for_jump_tmp_g[8:nrow(for_jump_tmp_g),3]),type = "l", main = "pca_location")
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3,cex = 2)
matplot(for_jump_tmp_g,type = "l", main = "linear_location")
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3,cex = 2)

biplot(pca_up_jump)
legend("topright",legend = c("x","y","z"),lty = 1:3,col = 1:3,cex = 2)

biplot2d = function(walk_top){
  walk_top_g = getGlobalAccByMag(walk_top)
  walk_top_g_2d = walk_top_g[,1:2]
  pca_model_walk = prcomp(walk_top_g_2d,center = F,scale. = F)
  walk_top_g_2d = predict(pca_model_walk, newdata = walk_top_g_2d)
  walk_top_g_pca = cbind(walk_top_g_2d, walk_top_g[,3])
  biplot(pca_model_walk)
}
biplot2d(walk_top[,])
biplot2d(run_top[,])
biplot2d(walk_legr[,])
biplot2d(walk_legl[,])
biplot2d(run_legr[,])
biplot2d(run_legl[,])
biplot2d(forjump_legr[,])
biplot2d(forjump_legl[,])

biplot2d(forjump_legr[100:300,])
biplot2d(forjump_legl[100:300,])
biplot2d(forjump_legl[200:400,])
biplot2d(forjump_legr[200:400,])
biplot2d(forjump_legr[1:500,])
biplot2d(forjump_legr[500:800,])

biplot2d(upjump_top[100:300,])
