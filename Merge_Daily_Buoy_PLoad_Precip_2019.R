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
rain19 = subset(rain1,subset=(year == 2019))

# load Pload
load('Ploads_PB+YP_2019-20.Rdata')
print(Pload[1,])
Pload$PBYW = Pload$PB.kg.d + Pload$YW.kg.d

# match the year
PL19 = subset(Pload,subset=(year==2019))
PL19$idoy = PL19$doy

# for these drivers, keep 1 March - 30 Sept, doy 61 - 274
train19 = subset(rain19,subset=(idoy>=61 & idoy<=274))
tPL19 = subset(PL19,subset=(idoy>=61 & idoy<=274))

# merge PL and precip
d1 = as.data.frame(train19)
d2 = as.data.frame(tPL19)
PLppt0 = merge(d1,d2,by=c('year','idoy'))
# sort
PLppt19 = PLppt0[with(PLppt0, order(idoy)), ] # sort

# load daily buoy
# Save cleaned-up Mendota 1-minute data frames
# Each data frame has year, integer DOY, decimal DOY, pigment, log10 pigment-minimum+1
#save(Me19,dark19,daily19,file='Me_Buoy_2019.Rdata')
load(file='Me_Buoy_2019.Rdata')
print('daily buoy variates and doy range',quote=F)
print(daily19[1,])
print('doy range of buoy data',quote=F)
print(range(daily19$idoy),quote=F)
print('doy range of PLoad & ppt',quote=F)
print(range(PLppt19$idoy),quote=F)

# For the drivers + buoy variates use 15 March - 30 Sept,
#  doy 136-274
datall0 = merge(daily19,PLppt19,by=c('year','idoy'))
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
all19 = datall2

save(all19,file='Buoy+PLppt_daily19.Rdata')
