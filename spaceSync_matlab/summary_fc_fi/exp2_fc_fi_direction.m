%this script 
%1.draw vector arrows of fc and fi-fc in 2d
%2.draw magnitude plot of fc and fi - fc

s1_fc = importdata('glass_fc_direction.csv')%fc direction unit vector
s2_fc = importdata('top_fc_direction.csv')
s3_fc = importdata('leftHand_fc_direction.csv')
s4_fc = importdata('rightHand_fc_direction.csv')
s5_fc = importdata('leftPants_fc_direction.csv')
s6_fc = importdata('rightPants_fc_direction.csv')

s1_fin = importdata('glass_fin_vector_magnitude.csv')%fi - fc vector(column 1-3) and fi-fc magnitude(column 4) and fc magnitude(column 5)
s2_fin = importdata('top_fin_vector_magnitude.csv')
s3_fin = importdata('leftHand_fin_vector_magnitude.csv')
s4_fin = importdata('rightHand_fin_vector_magnitude.csv')
s5_fin = importdata('leftPants_fin_vector_magnitude.csv')
s6_fin = importdata('rightPants_fin_vector_magnitude.csv')

%draw vector arrows of fc and fi-fc in 2d
%blue represent fc
%red represent fi-fc
figure(1)
subplot(2,3,1)
quiver(0,0,0.5,5,'r','LineWidth',3)
hold on
s = size(s1_fin.data)
quiver(repmat(0,50,1),repmat(0,50,1),s1_fin.data(1:5:250,1),s1_fin.data(1:5:250,2),'b')
xlim([-6 6])
ylim([-6 6])
title('s1 fi-noise fc direction')

subplot(2,3,2)
quiver(0,0,0.5,5,'r','LineWidth',3)
hold on
s = size(s2_fin.data)
quiver(repmat(0,50,1),repmat(0,50,1),s2_fin.data(1:5:250,1),s2_fin.data(1:5:250,2),'b')
xlim([-6 6])
ylim([-6 6])
title('s2 fi-noise fc direction')

subplot(2,3,3)
quiver(0,0,0.5,5,'r','LineWidth',3)
hold on
s = size(s3_fin.data)
quiver(repmat(0,50,1),repmat(0,50,1),s3_fin.data(1:5:250,1),s3_fin.data(1:5:250,2),'b')
xlim([-6 6])
ylim([-6 6])
title('s3 fi-noise fc direction')

subplot(2,3,4)
quiver(0,0,0.5,5,'r','LineWidth',3)
hold on
s = size(s4_fin.data)
quiver(repmat(0,50,1),repmat(0,50,1),s4_fin.data(1:5:250,1),s4_fin.data(1:5:250,2),'b')
xlim([-6 6])
ylim([-6 6])
title('s4 fi-noise fc direction')

subplot(2,3,5)
quiver(0,0,0.5,5,'r','LineWidth',3)
hold on
s = size(s5_fin.data)
quiver(repmat(0,50,1),repmat(0,50,1),s5_fin.data(1:5:250,1),s5_fin.data(1:5:250,2),'b')
xlim([-6 6])
ylim([-6 6])
title('s5 fi-noise fc direction')

subplot(2,3,6)
quiver(0,0,0.5,5,'r','LineWidth',3)
hold on
s = size(s6_fin.data)
quiver(repmat(0,50,1),repmat(0,50,1),s6_fin.data(1:5:250,1),s6_fin.data(1:5:250,2),'b')
xlim([-6 6])
ylim([-6 6])
title('s6 fi-noise fc direction')

%draw magnitude plot of fc and fi - fc
figure(7)
subplot(7,1,1)
plot(s1_fin.data(1:150,5))
xlabel('Sampling Sequence (50Hz)','FontSize',10);
ylabel('Accelerometer Data (m/s^2)','FontSize',10);
ylim([0 5])
title('fc magnitude')
subplot(7,1,2)
plot(s1_fin.data(1:150,4))
xlabel('Sampling Sequence (50Hz)','FontSize',10);
ylabel('Accelerometer Data (m/s^2)','FontSize',10);
ylim([0 5])
title('s1 fi noise magnitude')


subplot(7,1,3)
plot(s2_fin.data(1:150,4))
xlabel('Sampling Sequence (50Hz)','FontSize',10);
ylabel('Accelerometer Data (m/s^2)','FontSize',10);
ylim([0 5])
title('s2 fi noise magnitude')


subplot(7,1,4)
plot(s3_fin.data(1:150,4))
xlabel('Sampling Sequence (50Hz)','FontSize',10);
ylabel('Accelerometer Data (m/s^2)','FontSize',10);
ylim([0 5])
title('s3 fi noise magnitude')


subplot(7,1,5)
plot(s4_fin.data(1:150,4))
xlabel('Sampling Sequence (50Hz)','FontSize',10);
ylabel('Accelerometer Data (m/s^2)','FontSize',10);
ylim([0 5])
title('s4 fi noise magnitude')


subplot(7,1,6)
plot(s5_fin.data(1:150,4))
xlabel('Sampling Sequence (50Hz)','FontSize',10);
ylabel('Accelerometer Data (m/s^2)','FontSize',10);
ylim([0 5])
title('s5 fi noise magnitude')

subplot(7,1,7)
plot(s6_fin.data(1:150,4))
xlabel('Sampling Sequence (50Hz)','FontSize',10);
ylabel('Accelerometer Data (m/s^2)','FontSize',10);
ylim([0 5])
title('s6 fi noise magnitude')



