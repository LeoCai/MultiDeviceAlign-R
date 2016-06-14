function myerrorbar_group(model_series,model_error,ylable,title_name)

%model_series = [10 40 80 100 60;  30 60 100 100 20];

%model_error = [1 4 8 2 5; 3 6 10 2 4];

h = bar(model_series);

set(h,'BarWidth',1);    % The bars will now touch each other

set(gca,'YGrid','on')

set(gca,'GridLineStyle','-')

%set(gca,'XTicklabel','Modelo1|Modelo2|Modelo3')

set(get(gca,'YLabel'),'String',ylable)

lh = legend('s1','s2','s3','s4','s5','s6');

set(lh,'Location','BestOutside','Orientation','horizontal')

hold on;

numgroups = size(model_series, 1); 

numbars = size(model_series, 2); 

groupwidth = min(0.8, numbars/(numbars+1.5));

title(title_name)
for i = 1:numbars

      % Based on barweb.m by Bolu Ajiboye from MATLAB File Exchange

      x = (1:numgroups) - groupwidth/2 + (2*i-1) * groupwidth / (2*numbars);  % Aligning error bar with individual bar

      errorbar(x, model_series(:,i), model_error(:,i), 'k', 'linestyle', 'none');

end
