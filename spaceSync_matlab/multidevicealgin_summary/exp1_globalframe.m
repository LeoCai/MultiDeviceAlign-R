xglobal = importdata('xglobal.csv') %each column: s1_global_x, s2_global_x..... s6_global_x
yglobal = importdata('yglobal.csv')
zglobal = importdata('zglobal.csv')

figure(1)
plot(xglobal.data(1:100,[1 2 3 6]))
title('xglobal forward')
legend('s1','s2', 's3','s6','FontSize',10);
xlabel('Sampling Sequence (50Hz)','FontSize',14);
ylabel('Accelerometer Data (m/s^2)','FontSize',14);
title('xglobal','FontSize',14);

figure(2)
plot(yglobal.data(1:100,[1 2 3 6]))
title('yglobal left-right')
legend('s1','s2', 's3','s6','FontSize',10);
xlabel('Sampling Sequence (50Hz)','FontSize',14);
ylabel('Accelerometer Data (m/s^2)','FontSize',14);
title('yglobal','FontSize',14);

figure(3)
plot(zglobal.data(1:100,[1 2 3 6]))
title('zglobal vertical')
legend('s1','s2', 's3','s6','FontSize',10);
xlabel('Sampling Sequence (50Hz)','FontSize',14);
ylabel('Accelerometer Data (m/s^2)','FontSize',14);
title('zglobal','FontSize',14);