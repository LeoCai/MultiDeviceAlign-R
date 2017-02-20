test_slice_integral = function(filted_mag_data_f, filted_mag_data_l, gyr_data,  selected_index, dt, tag = "", writeFile = F){
  if(writeFile) png(paste("./new_exp/imags/", tag, ".png", sep = ""), width = 1024, height = 1024)
  par(mfrow = c(4,1))
  # plot(temp_gyr_z[selected_index], type="l", main="gyr_filted")
  plot(gyr_data[selected_index,2], type="l", main = "GYROSCOPE", cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5, xlab = "index(50HZ)", ylab = "angle velocity")
  # plot(liData[selected_index,2], type="l")
  inte_pos_f = abs(integralAcc(integralAcc(filted_mag_data_f[selected_index], dt[selected_index]), dt[selected_index]))
  inte_pos_l = abs(integralAcc(integralAcc(filted_mag_data_l[selected_index], dt[selected_index]), dt[selected_index]))
  
  inte_v_f = integralAcc(filted_mag_data_f[selected_index], dt[selected_index])
  inte_v_l = integralAcc(filted_mag_data_l[selected_index], dt[selected_index])
  
  matplot(cbind(filted_mag_data_f, filted_mag_data_l)[selected_index, ], type="l", main = "MAG GLOBAL ACCEROMETER" ,cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5, xlab = "index(50HZ)", ylab = "accerometer" )
  legend("topleft", c("forward", "right"), col = 1:2,lty = 1 ,cex = 2) 
  matplot(cbind(inte_v_f, inte_v_l), type="l", main = "VELOCITY",cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5, xlab = "index(50HZ)", ylab = "velocity")
  legend("topleft", c("forward", "right"), col = 1:2,lty = 1, cex = 2)
  matplot(cbind(inte_pos_f, inte_pos_l), type="l", main = "DISPLACEMENT",cex.main = 2, cex.lab = 2, lty=1, lwd = 1.5, xlab = "index(50HZ)", ylab = "displacement")
  legend("topleft", c("forward", "right"), col = 1:2,lty = 1, cex = 2)
  par(mfrow = c(1,1))
  if(writeFile) dev.off()
  return(c(inte_pos_f[length(inte_pos_f)], inte_pos_l[length(inte_pos_l)]))
}