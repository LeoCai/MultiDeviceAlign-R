SourceData=importdata('LeftPants_3.csv');
startIndex = 100;
endIndex = 800;
selectNum=endIndex-startIndex+1;
sycnData = SourceData.data(startIndex:endIndex,:);
acc = sycnData(:,1:3);
gyr = sycnData(:,10:12);
mag = sycnData(:,13:15);

figure(1);
plot(acc);
legend('Walking-Acc-X','Walking-Acc-Y', 'Walking-Acc-Z','FontSize',10);
xlabel('Sampling Sequence (50Hz)','FontSize',14);
ylabel('Accelerometer Data (m/s^2)','FontSize',14);
title('Walking Accelerometer (LeftPants-Local)','FontSize',14);

figure(2);
plot(gyr);
legend('Walking-Gyr-X','Walking-Gyr-Y', 'Walking-Gyr-Z','FontSize',10);
xlabel('Sampling Sequence (50Hz)','FontSize',14);
ylabel('Gyroscope Data (m/s^2)','FontSize',14);
title('Walking Gyroscope (LeftPants-Local)','FontSize',14);

figure(3);
plot(mag);
legend('Walking-Mag-X','Walking-Mag-Y', 'Walking-Mag-Z','FontSize',10);
xlabel('Sampling Sequence (50Hz)','FontSize',14);
ylabel('Magnetic Data (m/s^2)','FontSize',14);
title('Walking Magnetic (LeftPants-Local)','FontSize',14);
