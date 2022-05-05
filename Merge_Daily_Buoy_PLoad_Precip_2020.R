# Merge precip and P load dailies with buoy dailies
# SRC 2022-01-23

rm(list = ls())
graphics.off()

# read data
#rain0 = read.csv('DCRA_airtemp_precip_2019-20.csv')
#print(rain0[1,])
rain0 = read.csv('Daily_precip_2019-2021.csv')
print(rain0[1,])

# rename variates
year = rain0$year4
idoy = rain0$daynum # integer doy to match buoy file
maxT = rain0$max_air_temp_adjusted
minT = rain0$min_air_temp_adjusted
avgT = rain0$ave_air_temp_adjusted
pptmm = rain0$precip_raw_mm

# build new data frame
rain1 = as.data.frame(cbind(year,idoy,maxT,minT,avgT,pptmm)) 
rain20 = subset(rain1,subset=(year == 2020))

# load Pload
load('Ploads_PB+YP_2019-20.Rdata')
print(Pload[1,])
Pload$PBYW = Pload$PB.kg.d + Pload$YW.kg.d

# match the year
PL20 = subset(Pload,subset=(year==2020))
PL20$idoy = PL20$doy

# for these drivers, keep 1 March - 30 Sept, doy 61 - 274
train20 = subset(rain20,subset=(idoy>=61 & idoy<=274))
tPL20 = subset(PL20,subset=(idoy>=61 & idoy<=274))

# merge PL and precip
d1 = as.data.frame(train20)
d2 = as.data.frame(tPL20)
PLppt0 = merge(d1,d2,by=c('year','idoy'))
# sort
PLppt20 = PLppt0[with(PLppt0, order(idoy)), ] # sort

# load daily buoy
# Save cleaned-up Mendota 1-minute data frames
# Each data frame has year, integer DOY, decimal DOY, pigment, log10 pigment-minimum+1
#save(Me20,dark20,daily20,file='Me_Buoy_2020.Rdata')
load(file='Me_Buoy_2020.Rdata')
print('daily buoy variates and doy range',quote=F)
print(daily20[1,])
print(range(daily20$idoy),quote=F)
print('doy range of PLoad & ppt',quote=F)
print(range(PLppt20$idoy),quote=F)

# For the drivers + buoy variates use 15 March - 30 Sept,
#  doy 136-274
datall0 = merge(daily20,PLppt20,by=c('year','idoy'))
# check the sort
datall1 = datall0[with(datall0, order(idoy)), ]
# remove lines with missing data
datall2 = na.omit(datall0)
# compare before and after na.omit
print('dimensions before and after removing lines with NA',quote=F)
print(dim(datall0),quote=F)
print(dim(datall2),quote=F)
print('',quote=F)
print('columns of final file',quote=F)
print(datall2[1,],quote=F)

# rename final merged file and save
all20 = datall2

save(all20,file='Buoy+PLppt_daily20.Rdata')
