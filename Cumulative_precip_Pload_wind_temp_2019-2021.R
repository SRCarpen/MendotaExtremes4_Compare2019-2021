# Plot daily drivers 2019-2021

rm(list = ls())
graphics.off()

# load data

load(file='Buoy+PLppt_daily19.Rdata')
print(all19[1,])

load(file='Buoy+PLppt_daily20.Rdata')
print(all20[1,])

load(file='Buoy+PLppt_daily21.Rdata')
print(all21[1,])

# -------------------------------------------------------------

# How correlated are chl, bga, and their dark homologs?
Chls = c(all19$lChl,all20$lChl,all21$lChl)
BGAs = c(all19$lBGA,all20$lBGA,all21$lBGA)
dChls = c(all19$dark_lChl,all20$dark_lChl,all21$dark_lChl)
dBGAs = c(all19$dark_lBGA,all20$dark_lBGA,all21$dark_lBGA)

# make data frame
pigs = as.data.frame(cbind(Chls,BGAs,dChls,dBGAs))

# correlations
cormat = cor(pigs,use='pairwise.complete.obs')
print('correlations of pigments',quote=F)
print(round(cormat,3),quote=F)

windows(width=10,height=6)
par(mfrow=c(1,1),mar=c(4, 4.2, 3, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
xrange = range(all19$idoy,all20$idoy,all21$idoy)
yrange = range(0,sum(all19$pptmm),sum(all20$pptmm),sum(all21$pptmm))
plot(all19$idoy,cumsum(all19$pptmm),type='l',lwd=3,col='red',xlim=xrange,ylim=yrange,
     xlab='Day of Year',ylab='Precipitation, mm',
     main='Cumulative Precipitation 15 March - 30 September')
points(all20$idoy,cumsum(all20$pptmm),type='l',lwd=3,col='mediumorchid')
points(all21$idoy,cumsum(all21$pptmm),type='l',lwd=3,col='blue')
legend('topleft',legend=c('2019','2020','2021'),lwd=c(3,3,3),
       col=c('red','mediumorchid','blue'),cex=1.5,bty='n')

windows(width=10,height=6)
par(mfrow=c(1,1),mar=c(4, 4.2, 3, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
xrange = range(all19$idoy,all20$idoy,all21$idoy)
yrange = range(0,(all19$pptmm),(all20$pptmm),(all21$pptmm))
plot(all19$idoy,(all19$pptmm),type='l',lwd=2,col='red',xlim=xrange,ylim=yrange,
     xlab='Day of Year',ylab='Precipitation, mm')
points(all20$idoy,(all20$pptmm),type='l',lwd=2,col='mediumorchid')
points(all21$idoy,(all21$pptmm),type='l',lwd=2,col='blue')

windows(width=10,height=6)
par(mfrow=c(1,1),mar=c(4, 4.2, 3, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
xrange = range(all19$idoy,all20$idoy,all21$idoy)
yrange = range(0,(all19$wtemp),(all20$wtemp),(all21$wtemp))
plot(all19$idoy,(all19$wtemp),type='l',lwd=2,col='red',xlim=xrange,ylim=yrange,
     xlab='Day of Year',ylab='Water Temperature')
points(all20$idoy,(all20$wtemp),type='l',lwd=2,col='mediumorchid')
points(all21$idoy,(all21$wtemp),type='l',lwd=2,col='blue')

degreedays = function(x) {   # calculate cumulative degree days
  d15 = x - 15
  d15 = ifelse(d15 < 0,0,d15)
  cd15 = cumsum(d15)
  return(cd15)
}

# cumulative degree days
cdd19 = degreedays(all19$wtemp)
cdd20 = degreedays(all20$wtemp)
cdd21 = degreedays(all21$wtemp)

windows(width=10,height=6)
par(mfrow=c(1,1),mar=c(4, 4.2, 3, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
xrange = range(all19$idoy,all20$idoy,all21$idoy)
yrange = range(0,cdd19,cdd20,cdd21)
plot(all19$idoy,cdd19,type='l',lwd=2,col='red',xlim=xrange,ylim=yrange,
     xlab='Day of Year',ylab='Cumulative Degree Days',
     main='Cumulative Degree Days > 15')
points(all20$idoy,cdd20,type='l',lwd=2,col='mediumorchid')
points(all21$idoy,cdd21,type='l',lwd=2,col='blue')
legend('topleft',legend=c('2019','2020','2021'),lwd=c(3,3,3),
       col=c('red','mediumorchid','blue'),cex=1.5,bty='n')

windows(width=10,height=6)
par(mfrow=c(1,1),mar=c(4, 4.2, 3, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
xrange = range(all19$idoy,all20$idoy,all21$idoy)
yrange = range(0,sum(all19$v.wind),sum(all20$v.wind),sum(all21$v.wind))
plot(all19$idoy,cumsum(all19$v.wind),type='l',lwd=2,col='red',xlim=xrange,ylim=yrange,
     xlab='Day of Year',ylab='Wind Velocity, m/s',
     main='Cumulative Velocity 15 March - 30 September')
points(all20$idoy,cumsum(all20$v.wind),type='l',lwd=2,col='mediumorchid')
points(all21$idoy,cumsum(all21$v.wind),type='l',lwd=2,col='blue')

windows(width=10,height=6)
par(mfrow=c(1,1),mar=c(4, 4.2, 3, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
xrange = range(all19$idoy,all20$idoy,all21$idoy)
yrange = range(cumsum(all19$c.W),cumsum(all20$c.W),cumsum(all21$c.W))
plot(all19$idoy,cumsum(all19$c.W),type='l',lwd=2,col='red',xlim=xrange,ylim=yrange,
     xlab='Day of Year',ylab='Westing',
     main='Cumulative Westing 15 March - 30 September')
points(all20$idoy,cumsum(all20$c.W),type='l',lwd=2,col='mediumorchid')
points(all21$idoy,cumsum(all21$c.W),type='l',lwd=2,col='blue')
abline(h=0,lty=2,lwd=2)

windows(width=10,height=6)
par(mfrow=c(1,1),mar=c(4, 4.2, 3, 2) + 0.1,cex.axis=1.6,cex.lab=1.6)
xrange = range(all19$idoy,all20$idoy,all21$idoy)
yrange = range(cumsum(all19$c.N),cumsum(all20$c.N),cumsum(all21$c.N))
plot(all19$idoy,cumsum(all19$c.N),type='l',lwd=2,col='red',xlim=xrange,ylim=yrange,
     xlab='Day of Year',ylab='Northing',
     main='Cumulative Northing 15 March - 30 September')
points(all20$idoy,cumsum(all20$c.N),type='l',lwd=2,col='mediumorchid')
points(all21$idoy,cumsum(all21$c.N),type='l',lwd=2,col='blue')
abline(h=0,lty=2,lwd=2)

# COMPOSITE
windows(width=8,height=12)
par(mfrow=c(4,1),mar=c(2, 4.3, 1, 2) + 0.1,cex.axis=1.8,cex.lab=1.8)
xrange = range(all19$idoy,all20$idoy,all21$idoy)
yrange = range(0,sum(all19$pptmm),sum(all20$pptmm),sum(all21$pptmm))
plot(all19$idoy,cumsum(all19$pptmm),type='l',lwd=3,col='red',xlim=xrange,ylim=yrange,
     xlab=' ',ylab='Precipitation, mm',
     main='Cumulative Daily Data 15 March - 30 September')
points(all20$idoy,cumsum(all20$pptmm),type='l',lwd=3,col='mediumorchid')
points(all21$idoy,cumsum(all21$pptmm),type='l',lwd=3,col='blue')
legend(x=145,y=600,legend=c('2019','2020','2021'),lwd=c(3,3,3),
       col=c('red','mediumorchid','blue'),cex=1.5,bty='n')
text(x=135,y=560,'A',cex=1.8)
#
yrange = range(0,sum(all19$PBYW),sum(all20$PBYW) )#,sum(all21$pptmm))
plot(all19$idoy,cumsum(all19$PBYW),type='l',lwd=3,col='red',xlim=xrange,ylim=yrange,
     xlab=' ',ylab='P Load, kg/d')
points(all20$idoy,cumsum(all20$PBYW),type='l',lwd=3,col='mediumorchid')
points(all21$idoy,cumsum(all21$PBYW),type='l',lwd=3,col='blue')
text(x=135,y=5000,'B',cex=1.8)
#
xrange = range(all19$idoy,all20$idoy,all21$idoy)
yrange = range(0,cdd19,cdd20,cdd21)
plot(all19$idoy,cdd19,type='l',lwd=2,col='red',xlim=xrange,ylim=yrange,
     xlab=' ',ylab='Degree Days > 15')
points(all20$idoy,cdd20,type='l',lwd=2,col='mediumorchid')
points(all21$idoy,cdd21,type='l',lwd=2,col='blue')
text(x=135,y=900,'C',cex=1.8)
#
par(mar=c(4.5, 4.3, 1, 2) + 0.1)
xrange = range(all19$idoy,all20$idoy,all21$idoy)
yrange = range(0,sum(all19$v.wind),sum(all20$v.wind),sum(all21$v.wind))
plot(all19$idoy,cumsum(all19$v.wind),type='l',lwd=2,col='red',xlim=xrange,ylim=yrange,
     xlab='Day of Year',ylab='Wind Velocity, m/s')
points(all20$idoy,cumsum(all20$v.wind),type='l',lwd=2,col='mediumorchid')
points(all21$idoy,cumsum(all21$v.wind),type='l',lwd=2,col='blue')
text(x=135,y=450,'D',cex=1.8)