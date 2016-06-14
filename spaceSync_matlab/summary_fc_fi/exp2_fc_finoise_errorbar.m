%each column means: "mean_forward","mean_right","sd_forward","sd_right"
%each row means: s1 s2 s3 s4 s5 s6 s7
normal_fcbar = importdata('normal_fcbar.csv')
speedup_fcbar = importdata('speedup_fcbar.csv')
normal_fibar = importdata('normal_fibar.csv')
speedup_fibar = importdata('speedup_fibar.csv')
normal_finbar = importdata('normal_finbar.csv')
speedup_finbar = importdata('speedup_finbar.csv')

figure(1)
model_series = [normal_fcbar.data(:,1)';  normal_fcbar.data(:,2)'];
model_error = [normal_fcbar.data(:,3)';  normal_fcbar.data(:,4)'];
myerrorbar_group(model_series,model_error,'normal fcbar')

figure(2)
model_series = [speedup_fcbar.data(:,1)';  speedup_fcbar.data(:,2)'];
model_error = [speedup_fcbar.data(:,3)';  speedup_fcbar.data(:,4)'];
myerrorbar_group(model_series,model_error,'normal fcbar')

figure(3)
model_series = [normal_fcbar.data(:,1)';  normal_fcbar.data(:,2)'];
model_error = [normal_fcbar.data(:,3)';  normal_fcbar.data(:,4)'];
myerrorbar_group(model_series,model_error,'normal fcbar')

figure(4)
model_series = [speedup_fibar.data(:,1)';  speedup_fibar.data(:,2)'];
model_error = [speedup_fibar.data(:,3)';  speedup_fibar.data(:,4)'];
myerrorbar_group(model_series,model_error,'speedup fibar')

figure(5)
model_series = [normal_finbar.data(:,1)';  normal_finbar.data(:,2)'];
model_error = [normal_finbar.data(:,3)';  normal_finbar.data(:,4)'];
myerrorbar_group(model_series,model_error,'normal finbar')

figure(6)
model_series = [speedup_finbar.data(:,1)';  speedup_finbar.data(:,2)'];
model_error = [speedup_finbar.data(:,3)';  speedup_finbar.data(:,4)'];
myerrorbar_group(model_series,model_error,'speedup finbar')