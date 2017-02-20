
algMean = function(multi_data, clientNum){
  fc_all = c()
  for (i in 1:clientNum){
    client_data = getClientData(multi_data, i-1)
    lacc_data = client_data[,LACC_INDEXES]
    fc = apply(lacc_data, 2, mean)
    fc_all = rbind(fc_all, fc)
  }
  return(fc_all)
}



