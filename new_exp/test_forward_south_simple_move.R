# source("rotate_max_target_func.R")
library(zoo)
m1 = read.csv("./new_exp/datas/run_walk_native_gravity/top/walk1.csv")
# m1$LinearAcc1[ abs(m1$LinearAcc1)<0.05 ] = 0

filter_lc = function(lc){
  lc[ abs(lc)<0.05 ] = 0
  new_lc = lc
  for(i in 2:length(lc)){
    s = i - 20; e = i + 20; if(s<1) s = 1; if(e>length(lc)) e = length(lc)
    
    avg_lc = mean(lc[s:e]); sd_lc = sd(lc[s:e])
    if(abs(lc[i]- avg_lc)> 1.5*sd_lc){
      new_lc[i] = 0
    }else{
      new_lc[i] = lc[i]
    }
  }
  # lc = rollapply(new_lc, 5, mean, partial = T)
  return(new_lc)
}

filted_lc = filter_lc(-m1$LinearAcc0)
li_ma_forwad = cbind(-m1$LinearAcc0, filted_lc )
matplot(li_ma_forwad[1:nrow(li_ma_forwad),],type="l")

integralAcc = function(accs, dt){
  kn = rep(0, length(accs))
  for(i in 2 : length(accs)){
      kn[i] = kn[i-1]
      kn[i] = kn[i-1] + (accs[i]+accs[i-1])*dt[i]/2
  }
  return(kn)
}
# li_ma_forwad  = rollapply(li_ma_forwad[50:nrow(li_ma_forwad),], 20, mean)
# plot(l1, type = "l")
par(mfrow = c(3,1))
matplot(li_ma_forwad[1:nrow(li_ma_forwad),],type="l", main = "accerometer")
legend("topleft", c("linear", "maglinear"), col = 1:2, lty = 1:2)
v1_2 = cbind(integralAcc(li_ma_forwad[,1], m1$dt), integralAcc(li_ma_forwad[,2], m1$dt))
matplot(v1_2,type="l", main = "velocity")
x1_2 = cbind(integralAcc(integralAcc(li_ma_forwad[,1],m1$dt), m1$dt), integralAcc(integralAcc(li_ma_forwad[,2],m1$dt), m1$dt))
matplot(x1_2,type="l", main = "displacement")


# t = c(0,-1, 2, -1, 1)
# dt = rep(1, 5)
# 
# plot(integralAcc(t, dt),type="l", main = "velocity")
