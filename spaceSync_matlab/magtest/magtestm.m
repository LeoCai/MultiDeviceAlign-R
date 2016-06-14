SourceData=importdata('MagTestIndoorFloor.csv');

x = SourceData(:,2)
y = SourceData(:,1)
z = SourceData(:,3)
[X,Y] = meshgrid(1:6,1:10)
Z = griddata(x,y,z,X,Y)
figure(1)
surf(X,Y,Z)
xlabel('x','FontSize',14);
ylabel('y','FontSize',14);
zlabel('angle between north and magnet vector','FontSize',14);
title('Indoor Floor 5','FontSize',14);


SourceData=importdata('MagTestIndoor427.csv');
x = SourceData(:,1)
y = SourceData(:,2)
z = SourceData(:,3)
[X,Y] = meshgrid(1:6,1:10)
Z = griddata(x,y,z,X,Y)
figure(2)
surf(X,Y,Z)
xlabel('x','FontSize',14);
ylabel('y','FontSize',14);
zlabel('angle between north and magnet vector','FontSize',14);
title('Indoor Room 427','FontSize',14);
