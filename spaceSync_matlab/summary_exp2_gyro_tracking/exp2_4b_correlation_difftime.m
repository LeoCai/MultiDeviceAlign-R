%each column means s1-s6
%each row means a experiment
%each value means correlation with ground trueth
cor_gyro_small_10 = importdata('cor_gyro_small_10.csv')
cor_gyro_small_30 = importdata('cor_gyro_small_30.csv')
cor_gyro_small_60 = importdata('cor_gyro_small_60.csv')
cor_complementary_small_10 = importdata('cor_complementary_small_10.csv')
cor_complementary_small_30 = importdata('cor_complementary_small_30.csv')
cor_complementary_small_60 = importdata('cor_complementary_small_60.csv')
cor_calibration_small_10 = importdata('cor_calibration_small_10.csv')
cor_calibration_small_30 = importdata('cor_calibration_small_30.csv')
cor_calibration_small_60 = importdata('cor_calibration_small_60.csv')

figure(1)
m = [mean(cor_gyro_small_10.data);mean(cor_gyro_small_30.data);mean(cor_gyro_small_60.data)]
s = [std(cor_gyro_small_10.data);std(cor_gyro_small_30.data);std(cor_gyro_small_60.data)]
myerrorbar_group(m,s,'similarity','Only By Gyrocope')
set(gca,'xticklabel',{'10s','30s','60s'})

figure(2)
m = [mean(cor_complementary_small_10.data);mean(cor_complementary_small_30.data);mean(cor_complementary_small_60.data)]
s = [std(cor_complementary_small_10.data);std(cor_complementary_small_30.data);std(cor_complementary_small_60.data)]
myerrorbar_group(m,s,'similarity','Complementary Filter1')
set(gca,'xticklabel',{'10s','30s','60s'})

figure(3)
m = [mean(cor_calibration_small_10.data);mean(cor_calibration_small_30.data);mean(cor_calibration_small_60.data)]
s = [std(cor_calibration_small_10.data);std(cor_calibration_small_30.data);std(cor_calibration_small_60.data)]
myerrorbar_group(m,s,'similarity','Complementary Filter2')
set(gca,'xticklabel',{'10s','30s','60s'})
