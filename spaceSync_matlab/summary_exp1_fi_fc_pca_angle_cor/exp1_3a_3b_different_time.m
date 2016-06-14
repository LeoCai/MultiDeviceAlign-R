%each column means s1-s6
%each row means a experiment
%each value means pca angle drift on the horizontal
angleResults_5 = importdata('angleResults_small_5.csv')
angleResults_15 = importdata('angleResults_small_15.csv')
angleResults_30 = importdata('angleResults_small_30.csv')
angles_5 = angleResults_5.data
angles_15 = angleResults_15.data
angles_30 = angleResults_30.data

mean_5 = []; std_5 = []
%remove abnormal data
for i = 1:6
    mean_5 = [mean_5 mean(angles_5(abs(angles_5(:,i))<90, i))]
    std_5 = [std_5 std(angles_5(abs(angles_5(:,i))<90, i))]
end

mean_15 = []; std_15 = []
%remove abnormal data
for i = 1:6
    mean_15 = [mean_15 mean(angles_15(abs(angles_15(:,i))<90, i))]
    std_15 = [std_15 std(angles_15(abs(angles_15(:,i))<90, i))]
end

mean_30 = []; std_30 = []
%remove abnormal data
for i = 1:6
    mean_30 = [mean_30 mean(angles_30(abs(angles_30(:,i))<90, i))]
    std_30 = [std_30 std(angles_30(abs(angles_30(:,i))<90, i))]
end

a = [mean_5;mean_15;mean_30]
b = [std_5;std_15;std_30]
figure(1)
myerrorbar_group(a,b,'angle drift(degree)','pca angle drift of different time window')
set(gca,'xticklabel',{'5s'; '15s'; '30s' })

%each column means s1-s6
%each row means a experiment
%each value means pca correlation with mag converted global data
corResults_5 = importdata('pcaCorResults_small_5.csv')
corResults_15 = importdata('pcaCorResults_small_15.csv')
corResults_30 = importdata('pcaCorResults_small_30.csv')
cors_5 = corResults_5.data
cors_15 = corResults_15.data
cors_30 = corResults_30.data

mean_5 = []; std_5 = []
%remove abnormal data
for i = 1:6
    mean_5 = [mean_5 mean(cors_5(abs(angles_5(:,i))<90, i))]
    std_5 = [std_5 std(cors_5(abs(angles_5(:,i))<90, i))]
end

mean_15 = []; std_15 = []
%remove abnormal data
for i = 1:6
    mean_15 = [mean_15 mean(cors_15(abs(angles_15(:,i))<90, i))]
    std_15 = [std_15 std(cors_15(abs(angles_15(:,i))<90, i))]
end

mean_30 = []; std_30 = []
%remove abnormal data
for i = 1:6
    mean_30 = [mean_30 mean(cors_30(abs(angles_30(:,i))<90, i))]
    std_30 = [std_30 std(cors_30(abs(angles_30(:,i))<90, i))]
end

a = [mean_5;mean_15;mean_30]
b = [std_5;std_15;std_30]

figure(2)
myerrorbar_group(a,b,'similarity','similarity between pca and magnet converted global data')
set(gca,'xticklabel',{'5s'; '15s'; '30s' })