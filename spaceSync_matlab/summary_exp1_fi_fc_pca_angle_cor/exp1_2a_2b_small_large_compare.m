%each column means s1-s6
%each row means a experiment
%each value means pca angle drift on the horizontal
angleResults_small = importdata('angleResults_small_15.csv')
angleResults_large = importdata('angleResults_large_15.csv')
angles_small = angleResults_small.data()
angles_large = angleResults_large.data()
mean_small = []; std_small = []
%remove abnormal data
for i = 1:6
    mean_small = [mean_small mean(angles_small(abs(angles_small(:,i))<90, i))]
    std_small = [std_small std(angles_small(abs(angles_small(:,i))<90, i))]
end

mean_large = []; std_large = []
%remove abnormal data
for i = 1:6
    mean_large = [mean_large mean(angles_large(abs(angles_large(:,i))<90, i))]
    std_large = [std_large std(angles_large(abs(angles_large(:,i))<90, i))]
end

m = [mean_small;mean_large]
s = [std_large;std_large]
figure(1)
myerrorbar_group(m,s,'angle drift (degree)','pca angle drift (small and large)')
set(gca,'xticklabel',{'small','large' })


pcaCorResults_small = importdata('pcaCorResults_small_15.csv')
pcaCorResults_large = importdata('pcaCorResults_large_15.csv')
cors_small = pcaCorResults_small.data()
cors_large = pcaCorResults_large.data()
mean_small = []; std_small = []
%remove abnormal data
for i = 1:6
    mean_small = [mean_small mean(cors_small(abs(angles_small(:,i))<90, i))]
    std_small = [std_small std(cors_small(abs(angles_small(:,i))<90, i))]
end

mean_large = []; std_large = []
%remove abnormal data
for i = 1:6
    mean_large = [mean_large mean(cors_large(abs(angles_large(:,i))<90, i))]
    std_large = [std_large std(cors_large(abs(angles_large(:,i))<90, i))]
end

m = [mean_small;mean_large]
s = [std_large;std_large]
figure(2)
myerrorbar_group(m,s,'similarity','similarity between pca and magnet converted global data')
set(gca,'xticklabel',{'small','large' })