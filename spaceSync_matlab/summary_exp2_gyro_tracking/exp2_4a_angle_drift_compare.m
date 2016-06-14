%each column means vertical drift by gyro, vertical drift by
%complementary1,by complementary2, magdrift by gyro, magdrift by
%complementary1, by complemenarty2(6 * 6 device * 24expnum)
%each row means a time index
%each value means angle drift
drift_small_60 = importdata('drifts_small_60.csv')
num = 2
tag = {'glass','top','lefthand','righthand','leftpants','rightpants'}
for i = 0:5
    dritf_vertical_1 = drift_small_60.data(:,num*36+1+6*i)
    dritf_vertical_2 = drift_small_60.data(:,num*36+2+6*i)
    dritf_vertical_3 = drift_small_60.data(:,num*36+3+6*i)
    dritf_mag_1 = drift_small_60.data(:,num*36+4+6*i)
    dritf_mag_2 = drift_small_60.data(:,num*36+5+6*i)
    dritf_mag_3 = drift_small_60.data(:,num*36+6+6*i)
    figure(i+1)
    subplot(2,1,1)
    plot([dritf_vertical_1 dritf_vertical_2 dritf_vertical_3])
    legend('Only By Gyroscope','Complementary1', 'Complementary2','FontSize',10);
    xlabel('Sampling Sequence (100Hz)','FontSize',14);
    ylabel('Angle Drift(degree)','FontSize',14);
    title([tag(i+1),' Angle Drift(Vertical)'],'FontSize',14);
    subplot(2,1,2)
    plot([dritf_mag_1 dritf_mag_2 dritf_mag_3])
    legend('Only By Gyroscope','Complementary1', 'Complementary2','FontSize',10);
    xlabel('Sampling Sequence (100Hz)','FontSize',14);
    ylabel('Angle Drift(degree)','FontSize',14);
    title([tag(i+1),' Angle Drift(Vertical)'],'FontSize',14);
end
