%each column means s1-s6
%each row means a experiment
%each value means correlation with ground trueth
cor_gyro_small_60 = importdata('cor_gyro_small_60.csv')
cor_gyro_large_60 = importdata('cor_gyro_large_60.csv')
cor_complementary_small_60 = importdata('cor_complementary_small_60.csv')
cor_complementary_large_60 = importdata('cor_complementary_large_60.csv')
cor_calibration_small_60 = importdata('cor_calibration_small_60.csv')
cor_calibration_large_60 = importdata('cor_calibration_large_60.csv')

figure(1)
m = [mean(cor_gyro_small_60.data);mean(cor_gyro_large_60.data)]
s = [std(cor_gyro_small_60.data);std(cor_gyro_large_60.data)]
myerrorbar_group(m,s,'similarity','Small Large Compare Only By Gyro')
set(gca,'xticklabel',{'small','large'})

figure(2)
m = [mean(cor_complementary_small_60.data);mean(cor_complementary_large_60.data)]
s = [std(cor_complementary_small_60.data);std(cor_complementary_large_60.data)]
myerrorbar_group(m,s,'similarity','Small Large Compare Complementary1')
set(gca,'xticklabel',{'small','large'})

figure(3)
m = [mean(cor_calibration_small_60.data);mean(cor_calibration_large_60.data)]
s = [std(cor_calibration_small_60.data);std(cor_calibration_large_60.data)]
myerrorbar_group(m,s,'similarity','Small Large Compare Complementary2')
set(gca,'xticklabel',{'small','large'})
