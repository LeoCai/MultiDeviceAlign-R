require(stats)
#Domain setup

dt <- 0.01 #s
n <- T/dt
df <- 1/T
t <- seq(0,T,by=dt) #also try ts function

#CREATE OUR TIME SERIES DATA
y <- 10*sin(2*pi*1.5*t) +4* sin(2*pi*20*t)

#CREATE OUR FREQUENCY ARRAY
f <- 1:length(t)/T
#FOURIER TRANSFORM WORK
Y <- fft(y)
mag <- sqrt(Re(Y)^2+Im(Y)^2)*2/n
phase <- atan(Im(Y)/Re(Y))
Yr <- Re(Y)
Yi <- Im(Y)
fft(Y,inverse = T)
y
#PLOTTING
layout(matrix(c(1,2), 2, 1, byrow = TRUE))
plot(t,y,type="l",xlim=c(0,T))
plot(f[1:length(f)/2],mag[1:length(f)/2],type="l")
