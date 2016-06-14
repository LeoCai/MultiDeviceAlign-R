SourceData=importdata('Glass_3.csv');
startIndex = 20;
endIndex = 200;
selectNum=endIndex-startIndex+1;
sycnData = SourceData.data(startIndex:endIndex,:);
acc = sycnData(:,1:3);
gyr = sycnData(:,4:6);
mag = sycnData(:,7:9);

figure(1);
plot(acc);
legend('Walking-Acc-X','Walking-Acc-Y', 'Walking-Acc-Z','FontSize',10);
xlabel('Sampling Sequence (10Hz)','FontSize',14);
ylabel('Accelerometer Data (m/s^2)','FontSize',14);
title('Walking Accelerometer (Glass-Local)','FontSize',14);

figure(2);
plot(gyr);
legend('Walking-Gyr-X','Walking-Gyr-Y', 'Walking-Gyr-Z','FontSize',10);
xlabel('Sampling Sequence (50Hz)','FontSize',14);
ylabel('Gyroscope Data (m/s^2)','FontSize',14);
title('Walking Gyroscope (Glass-Local)','FontSize',14);

figure(3);
plot(mag);
legend('Walking-Mag-X','Walking-Mag-Y', 'Walking-Mag-Z','FontSize',10);
xlabel('Sampling Sequence (50Hz)','FontSize',14);
ylabel('Magnetic Data (m/s^2)','FontSize',14);
title('Walking Magnetic (Glass-Local)','FontSize',14);
