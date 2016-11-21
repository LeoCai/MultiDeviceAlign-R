library(Rwave) #contains the cwt() transformation function

#Computes the scale values for a given number of octaves and voices
scales=function(numOctave,numVoice){
  scale=vector(length=numOctave*numVoice)
  scale.index=1
  for(octave in 0:(numOctave-1)){
    for(voice in 0:(numVoice-1)){
      scale[scale.index]=2^(octave+(voice/numVoice))
      scale.index=scale.index+1
    }
  }
  scale
}

#Computes the corresponding frequency values for a given vector of scales, center frequency, and sample period
scaleToFreq=function(scale,center,samplePeriod,plot=T){
  freq=(center)/(scale*(samplePeriod*2));
  freq
}

#.8125 is the default center frequency for the Morlet wavelet
scale2freq=scaleToFreq(scales(6,10),.8125,1/480) #store these values for later use

s.freq = 480 #sample frequency
t = seq(1/s.freq,5,by=1/s.freq)#time steps in test signal below
y=sinpi(2*3*t)+sinpi(2*6*t)+sinpi(2*11*t)+sinpi(2*100*t) #the middle digit in each function represents frequency
z=sinpi(2*23*t)
z[50:150]=0 #These zeroes will represent discontinuities at 23Hz
z[300:480]=0
f=y+z #signal to be analyzed

plot(t,f,type="l",main="Example Signal")

octaves=6 #octaves in scale values
voices=10 #voices per octave
omega=2*pi*.8125 #desired center frequency for Morlet wavelet
coef = cwt(f,noctave=octaves,nvoice=voices,w0=omega,twoD=TRUE,plot=FALSE) #Store wavelet coefficients

t.axis=seq(0,(length(f)-1)/s.freq,by=1/s.freq) #a time axis to be used in later plots
scale=1:dim(coef)[2] #a dummy axis to be used to display decreasing frequencies
names(scale)=floor(scaleToFreq(scales(octaves,voices),0.8125,1/(s.freq),plot=F)) #the names function was used to avoid problems with decreasing frequency values on the y axis
matplot(x=t.axis,Re(coef),ylab="Spectrum",xlab="Time",type="l",main="Wavelet Decomposition")

#create an image plot of the squared values of the coefficients
image(t.axis,scale,abs(coef)^2,yaxt="n",ylab="Frequency",xlab="Time",main="Scalogram",col=grey(seq(0,1,length=1024)))
axis(2,at=seq(1,(octaves*voices)),labels=names(scale))

f = walk_legr$ResultantAcc

wavePlot = function(f,octaves,voices,omega, tag=""){
  s.freq = 50 
  coef = cwt(f,noctave=octaves,nvoice=voices,w0=omega,twoD=TRUE,plot=FALSE) #Store wavelet coefficients
  t.axis=seq(0,(length(f)-1)/s.freq,by=1/s.freq) #a time axis to be used in later plots
  scale=1:dim(coef)[2] #a dummy axis to be used to display decreasing frequencies
  names(scale)=floor(scaleToFreq(scales(octaves,voices),0.8125,1/(s.freq),plot=F)) #the names function was used to avoid problems with decreasing frequency values on the y axis
  # matplot(x=t.axis,Re(coef),ylab="Spectrum",xlab="Time",type="l",main="Wavelet Decomposition")
  image(t.axis,scale,abs(coef)^2,yaxt="n",ylab="Frequency",xlab="Time",main=tag,col=grey(seq(0,1,length=1024)))
  axis(2,at=seq(1,(octaves*voices)),labels=names(scale))
}
par(mfrow = c(1,1))
wavePlot(run_legl$ConvertedData1,4,3,2, tag = "legr")
wavePlot(run_legr$ConvertedData1,5,10,2, tag = "legl")
wavePlot(run_top$ConvertedData1,5,10,2, tag = "top")

library(stats)
plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  points(ts, harmonic.trajectory, type="l", col=color)
}
fft_run_leg  = fft(walk_top$LinearAcc2)
plot.frequency.spectrum(fft_run_leg, xlimits=c(0,5))

fft_run_leg  = fft(run_top$LinearAcc2)
plot.frequency.spectrum(fft_run_leg, xlimits=c(0,5))

fft_run_leg  = fft(run_top$LinearAcc2)
plot.frequency.spectrum(fft_run_leg, xlimits=c(0,50))

fft_run_leg  = fft(run_legl$LinearAcc2)
plot.frequency.spectrum(fft_run_leg, xlimits=c(0,50))

fft_run_leg  = fft(run_legl$LinearAcc2)
plot.frequency.spectrum(fft_run_leg, xlimits=c(0,50))