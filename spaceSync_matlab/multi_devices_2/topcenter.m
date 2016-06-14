SourceData=importdata('topcenter.csv');
startIndex = 400;
endIndex = 1000;
selectNum=endIndex-startIndex+1;
sycnData = SourceData.data(startIndex:endIndex,:);
localAcc =  sycnData(:,4:6);
%globalByMagnetAcc = sycnData(:, 16:18);

figure(1);
plot(localAcc);
legend('Walking-Acc-Left','Walking-Acc-Up', 'Walking-Acc-Forward','FontSize',10);
xlabel('Sampling Sequence (100Hz)','FontSize',14);
ylabel('Accelerometer Data (m/s^2)','FontSize',14);
title('Walking Accelerometer (Top-Center-Local)','FontSize',14);

%figure(2);
%plot(globalByMagnetAcc);
%legend('Walking-Acc-Forward','Walking-Acc-Left', 'Walking-Acc-Gravity','FontSize',10);
%xlabel('Sampling Sequence (100Hz)','FontSize',14);
%ylabel('Accelerometer Data (m/s^2)','FontSize',14);
%title('Walking Accelerometer (Top-Center-Global-By-Magnet)','FontSize',14);
