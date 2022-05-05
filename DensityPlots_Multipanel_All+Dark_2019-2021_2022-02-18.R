# Densities of buoy data 2200-0400

rm(list = ls())
graphics.off()

# read data
#save(Me20,dark20,daily20,file=fname) # example
load(file='Me_Buoy_2019.Rdata')
load(file='Me_Buoy_2020.Rdata')
load(file='Me_Buoy_2021.Rdata')

# Make fill colors for densities

# Function to tune transparency
# Alpha is transparency on 0-100 scale
makeTransparent<-function(someColor, alpha)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, 
        function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}
#
transcoral = makeTransparent('coral1',50)
transorchid = makeTransparent('orchid1',60)
transblue = makeTransparent('lightskyblue',75)

# write a function for multipanel density plots
densplot3 = function(v19,v20,v21,nx,xtext,mtext,cap) {  # ,legloc removed
  v19c = subset(v19,subset=(v19<cap))
  v20c = subset(v20,subset=(v20<cap))
  v21c = subset(v21,subset=(v21<cap))
  dens19 = density(v19c,bw='SJ',window="epanechnikov",n=nx,na.rm='T')
  dens20 = density(v20c,bw='SJ',window="epanechnikov",n=nx,na.rm='T')
  dens21 = density(v21c,bw='SJ',window="epanechnikov",n=nx,na.rm='T')
  xrange = range(dens19$x,dens20$x,dens21$x,na.rm=T)
  #xrange = c(0,cap)
  #
  windows()
  par(mfrow=c(3,1),mar=c(1, 4.5, 3, 1) + 0.1, cex.axis=1.8,cex.lab=1.8)
  plot(dens19$x,dens19$y,type='l',lwd=2,col='red',xlim=xrange,ylab='Density',
       xlab='',main=mtext)
  polygon(dens19$x,dens19$y,col=transcoral,border=NA)
  text(xrange[2]-0.1,max(dens19$y)*0.95,'2019',cex=1.8)
  #
  par(mar=c(1, 4.5, 2, 1) + 0.1)
  plot(dens20$x,dens20$y,type='l',lwd=2,col='mediumorchid',xlim=xrange,ylab='Density',
       xlab='',main='')
  polygon(dens20$x,dens20$y,col=transorchid,border=NA)
  text(xrange[2]-0.11,max(dens20$y)*0.95,'2020',cex=1.8)
  #
  par(mar=c(4, 4.5, 2, 1) + 0.1)
  plot(dens21$x,dens21$y,type='l',lwd=2,col='dodgerblue',xlim=xrange,ylab='Density',
       xlab=xtext,main='')
  polygon(dens21$x,dens21$y,col=transblue,border=NA)
  text(xrange[2]-0.1,max(dens21$y)*0.95,'2021',cex=1.8)
  #
  #legend(legloc,legend = c('2019','2020','2021'),lwd=c(2,2,2),
  #       col=c('red','mediumorchid','dodgerblue'),cex=1.5,bty='n')
}

#===============================================================================

densplot3(Me19$lBGA,Me20$lBGA,Me21$lBGA,nx=256,cap=1,
         xtext=c('log10 Phycocyanin'),mtext='Phycocyanin')#,legloc='topright')

densplot3(dark19$lBGA,dark20$lBGA,dark21$lBGA,nx=256,cap=1,
         xtext=c('log10 Phycocyanin'),mtext='Phycocyanin 2200-0400')#,legloc='topright')

densplot3(Me19$lChl,Me20$lChl,Me21$lChl,nx=256,cap=1,
         xtext=c('log10 Chlorophyll'),mtext='Chlorophyll')#,legloc='topright')

densplot3(dark19$lChl,dark20$lChl,dark21$lChl,nx=256,cap=1,
         xtext=c('log10 Chlorophyll'),mtext='Chlorophyll 2200-0400')#,legloc='topright')

windows()
par(mfrow=c(3,1),mar=c(3.8, 4, 1, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
plot(daily19$idoy,daily19$lBGA,type='l',lwd=2,col='blue',xlab='DoY 2019',
     ylab='log10 Phycocyanin')
grid()
plot(daily20$idoy,daily20$lBGA,type='l',lwd=2,col='blue',xlab='DoY 2020',
     ylab='log10 Phycocyanin')
grid()
plot(daily21$idoy,daily21$lBGA,type='l',lwd=2,col='blue',xlab='DoY 2021',
     ylab='log10 Phycocyanin')
grid()

windows()
par(mfrow=c(3,1),mar=c(3.8, 4, 1, 2) + 0.1, cex.axis=1.6,cex.lab=1.6)
plot(dark19$idoy,dark19$lBGA,type='l',lwd=2,col='blue',xlab='DoY 2019',
     ylab='log10 Phycocyanin',main='Phycocyanin 2200-0400')
grid()
plot(dark20$idoy,dark20$lBGA,type='l',lwd=2,col='blue',xlab='DoY 2020',
     ylab='log10 Phycocyanin')
grid()
plot(dark21$idoy,dark21$lBGA,type='l',lwd=2,col='blue',xlab='DoY 2021',
     ylab='log10 Phycocyanin')
grid()

