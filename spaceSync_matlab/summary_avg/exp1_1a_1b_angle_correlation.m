%each column means s1-s6
%each row means a experiment
%each value means pca angle drift on the horizontal
angleResults = importdata('angleResults_small_15.csv')
angles = angleResults.data(); 
a = []; b = []
%remove abnormal data
for i = 1:6
    a = [a mean(angles(:, i))]
    b = [b std(angles(:, i))]
end
figure(1);
bar(a,'b');
hold on;
errorbar(a,b,'k','LineStyle','none');
xlabel('Device','FontSize',14);
ylabel('Angle Drift (degree)','FontSize',14);
title('Horizontal Angle Drift By Average')
set(gca,'xticklabel',{'s1'; 's2'; 's3';'s4';'s5';'s6' })


%each column means s1-s6
%each row means a experiment
%each value means correlation between pca and magnet global data
corResults = importdata('pcaCorResults_small_15.csv')
cors = corResults.data(); 
%remove abnormal data
a = []; b = []
for i = 1:6
    a = [a mean(cors(:, i))]
    b = [b std(cors(:, i))]
end
figure(2);
bar(a,'b');
hold on;
errorbar(a,b,'k','LineStyle','none');
xlabel('Device','FontSize',14);
ylabel('Similarity','FontSize',14);
title('Similarity Between Average Converted Global Data and Magnet Conveted Global Data')
set(gca,'xticklabel',{'s1'; 's2'; 's3';'s4';'s5';'s6' })
