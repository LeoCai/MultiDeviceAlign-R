s1_drift = importdata('drift_GLASS.csv')
s2_drift = importdata('drift_TOP.csv')
s3_drift = importdata('drift_LEFTHAND.csv')
s4_drift = importdata('drift_RIGHTHAND.csv')
s5_drift = importdata('drift_LEFTPANTS.csv')
s6_drift = importdata('drift_RIGHTPANTS.csv')

figure(1)
subplot(2,1,1)
cdfplot(s1_drift.data(1:250,1))
hold on 
cdfplot(s1_drift.data(1:500,1))
hold on 
cdfplot(s1_drift.data(1:1000,1))
hold on 
cdfplot(s1_drift.data(1:2938,1))
legend('5s','10s','20s','60s','FontSize',6);
xlabel('Error(degree)','FontSize',14);
ylabel('CDF','FontSize',14);
title('s1 vertical drift','FontSize',14);
subplot(2,1,2)
cdfplot(s1_drift.data(1:250,2))
hold on 
cdfplot(s1_drift.data(1:500,2))
hold on 
cdfplot(s1_drift.data(1:1000,2))
hold on 
cdfplot(s1_drift.data(1:2938,2))
legend('5s','10s','20s','60s','FontSize',6);
xlabel('Error(degree)','FontSize',14);
ylabel('CDF ','FontSize',14);
title('s1 leftright drift','FontSize',14);

figure(2)
subplot(2,1,1)
cdfplot(s2_drift.data(1:250,1))
hold on 
cdfplot(s2_drift.data(1:500,1))
hold on 
cdfplot(s2_drift.data(1:1000,1))
hold on 
cdfplot(s2_drift.data(1:2938,1))
legend('5s','10s','20s','60s','FontSize',6);
xlabel('Error(degree)','FontSize',14);
ylabel('CDF','FontSize',14);
title('s2 vertical drift','FontSize',14);
subplot(2,1,2)
cdfplot(s2_drift.data(1:250,2))
hold on 
cdfplot(s2_drift.data(1:500,2))
hold on 
cdfplot(s2_drift.data(1:1000,2))
hold on 
cdfplot(s2_drift.data(1:2938,2))
legend('5s','10s','20s','60s','FontSize',6);
xlabel('Error(degree)','FontSize',14);
ylabel('CDF ','FontSize',14);
title('s2 leftright drift','FontSize',14);

figure(3)
subplot(2,1,1)
cdfplot(s3_drift.data(1:250,1))
hold on 
cdfplot(s3_drift.data(1:500,1))
hold on 
cdfplot(s3_drift.data(1:1000,1))
hold on 
cdfplot(s3_drift.data(1:2938,1))
legend('5s','10s','20s','60s','FontSize',6);
xlabel('Error(degree)','FontSize',14);
ylabel('CDF','FontSize',14);
title('s3 vertical drift','FontSize',14);
subplot(2,1,2)
cdfplot(s3_drift.data(1:250,2))
hold on 
cdfplot(s3_drift.data(1:500,2))
hold on 
cdfplot(s3_drift.data(1:1000,2))
hold on 
cdfplot(s3_drift.data(1:2938,2))
legend('5s','10s','20s','60s','FontSize',6);
xlabel('Error(degree)','FontSize',14);
ylabel('CDF ','FontSize',14);
title('s3 leftright drift','FontSize',14);

figure(4)
subplot(2,1,1)
cdfplot(s4_drift.data(1:250,1))
hold on 
cdfplot(s4_drift.data(1:500,1))
hold on 
cdfplot(s4_drift.data(1:1000,1))
hold on 
cdfplot(s4_drift.data(1:2938,1))
legend('5s','10s','20s','60s','FontSize',6);
xlabel('Error(degree)','FontSize',14);
ylabel('CDF','FontSize',14);
title('s4 vertical drift','FontSize',14);
subplot(2,1,2)
cdfplot(s4_drift.data(1:250,2))
hold on 
cdfplot(s4_drift.data(1:500,2))
hold on 
cdfplot(s4_drift.data(1:1000,2))
hold on 
cdfplot(s4_drift.data(1:2938,2))
legend('5s','10s','20s','60s','FontSize',6);
xlabel('Error(degree)','FontSize',14);
ylabel('CDF ','FontSize',14);
title('s4 leftright drift','FontSize',14);

figure(5)
subplot(2,1,1)
cdfplot(s5_drift.data(1:250,1))
hold on 
cdfplot(s5_drift.data(1:500,1))
hold on 
cdfplot(s5_drift.data(1:1000,1))
hold on 
cdfplot(s5_drift.data(1:2938,1))
legend('5s','10s','20s','60s','FontSize',6);
xlabel('Error(degree)','FontSize',14);
ylabel('CDF','FontSize',14);
title('s5 vertical drift','FontSize',14);
subplot(2,1,2)
cdfplot(s5_drift.data(1:250,2))
hold on 
cdfplot(s5_drift.data(1:500,2))
hold on 
cdfplot(s5_drift.data(1:1000,2))
hold on 
cdfplot(s5_drift.data(1:2938,2))
legend('5s','10s','20s','60s','FontSize',6);
xlabel('Error(degree)','FontSize',14);
ylabel('CDF ','FontSize',14);
title('s5 leftright drift','FontSize',14);

figure(6)
subplot(2,1,1)
cdfplot(s6_drift.data(1:250,1))
hold on 
cdfplot(s6_drift.data(1:500,1))
hold on 
cdfplot(s6_drift.data(1:1000,1))
hold on 
cdfplot(s6_drift.data(1:2938,1))
legend('5s','10s','20s','60s','FontSize',6);
xlabel('Error(degree)','FontSize',14);
ylabel('CDF','FontSize',14);
title('s6 vertical drift','FontSize',14);
subplot(2,1,2)
cdfplot(s6_drift.data(1:250,2))
hold on 
cdfplot(s6_drift.data(1:500,2))
hold on 
cdfplot(s6_drift.data(1:1000,2))
hold on 
cdfplot(s6_drift.data(1:2938,2))
legend('5s','10s','20s','60s','FontSize',6);
xlabel('Error(degree)','FontSize',14);
ylabel('CDF ','FontSize',14);
title('s6 leftright drift','FontSize',14);