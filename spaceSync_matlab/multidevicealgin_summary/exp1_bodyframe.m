xlocal = importdata('xlocal.csv') %each column: s1_x, s2_x..... s6_x
ylocal = importdata('ylocal.csv') %each column: s1_y, s2_y..... s6_y
zlocal = importdata('zlocal.csv') %each column: s1_z, s2_z..... s6_z
magnitudelocal = importdata('magnitudelocal.csv') %each column: s1_magnitude, s2_magnitude..... s6_magnitude

figure(1)
plot(xlocal.data(1:100,[1 2 3 6]))
title('xlocal')
legend('s1','s2', 's3','s6','FontSize',10);
xlabel('Sampling Sequence (50Hz)','FontSize',14);
ylabel('Accelerometer Data (m/s^2)','FontSize',14);
title('xlocal','FontSize',14);

figure(2)
plot(ylocal.data(1:100,[1 2 3 6]))
title('ylocal')
legend('s1','s2', 's3','s6','FontSize',10);
xlabel('Sampling Sequence (50Hz)','FontSize',14);
ylabel('Accelerometer Data (m/s^2)','FontSize',14);
title('ylocal','FontSize',14);

figure(3)
plot(zlocal.data(1:100,[1 2 3 6]))
title('zlocal')
legend('s1','s2', 's3','s6','FontSize',10);
xlabel('Sampling Sequence (50Hz)','FontSize',14);
ylabel('Accelerometer Data (m/s^2)','FontSize',14);
title('zlocal','FontSize',14);

figure(4)
plot(magnitudelocal.data(1:100,[1 2 3 6]))
title('magnitude')
legend('s1','s2', 's3','s6','FontSize',10);
xlabel('Sampling Sequence (50Hz)','FontSize',14);
ylabel('Accelerometer Data (m/s^2)','FontSize',14);
title('magnitude','FontSize',14);