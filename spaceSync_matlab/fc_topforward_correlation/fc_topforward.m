topforward=importdata('topforward.csv');
FC_Magnitude=importdata('FC_Magnitude.csv');
FC_Vector=importdata('FC_Vector.csv');

allData = [topforward,FC_Magnitude,FC_Vector]

plot(allData)
legend('topforwardt','FC_Magnitude', 'FC_Vector','FontSize',10);
xlabel('Sampling Sequence (100Hz)','FontSize',14);
ylabel('Accelerometer Data (m/s^2)','FontSize',14);
title('Fc topforward','FontSize',14);
