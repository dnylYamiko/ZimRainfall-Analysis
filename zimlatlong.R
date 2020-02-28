setwd("~Zimbabwe_Rain_Analysis_1981-2020")
##This script is analysing data from 
##https://iridl:ldeo.columbia.edu/SOURCES/UCSB/CHIRPS/v2p0/

require(fields)
require(ncdf4)
require(raster)
require(zoo)
require(xts)
require(ggplot2)

dat = nc_open("data2.nc")

#the data has 3 dimensions, precipitation[X,Y,T].
longitude = ncvar_get(dat,"X")
latitude = ncvar_get(dat,"Y")
time = ncvar_get(dat,"T")

time<-seq(as.Date("1981-01-01"),to=as.Date('2020-01-01'),"months")
time <- as.yearmon(time)

prec.array <- ncvar_get(dat,"precipitation")


#Heatmap for entire country
lati_rang = c(-22,-17)
longi_rang = c(26,32)

lati_ind = which(latitude >= lati_rang[1] & latitude <= lati_rang[2])
long_ind = which(longitude >= longi_rang[1] & longitude <= longi_rang[2])

prec_ts = ncvar_get(dat,"precipitation",start = c(long_ind[1],lati_ind[1],1),
                    count = c(length(long_ind),length(lati_ind),-1))
dim(prec_ts)

#This plots the heatmap for the lat and lon coordinates picked above
#the coordinates are for Zimbabwe and its showing rainful over the entire time period
image.plot(prec_ts[,,1])

#for the time series, we need the mean rainfall for each year. 
prec_ts=apply(prec_ts,3,mean,na.rm=T)

plot(time,prec_ts,type = 'l',col = "red",xlab="Time (Years)",ylab = "Mean Yearly Rainfall",
     main = " Time series for Zimbabwe Rainfall 1981-2020")
abline(h=mean(rain.array,na.rm = TRUE),col="purple",)
abline(h=sd(rain.array,na.rm = TRUE),col="black")
abline(h=min(rain.array,na.rm = TRUE),col="blue")
abline(h=min(rain.array,na.rm = TRUE),col = "yellow")

dev.copy(png,file="timeseries.png",width=520,height=520)
dev.off()
cat("\ntimeseries.png has been save in ",getwd())

nc.brick <- brick("data2.nc")

dim(nc.brick)
nc.df<-as.data.frame(nc.brick,xy=T)

dim(nc.df)

cat("\n The mean rainfall for 1980: ", mean(nc.df$X252.5,na.rm=TRUE))

#Picked Gutu(-19.584361,30.857374), as a location of interest
prec.array <- ncvar_get(dat,"precipitation")
lat.gutu<-latitude[which.min(abs(latitude-(-19.584361)))]
lon.gutu<-longitude[which.min(abs(longitude - 30.857374))]
gutu.rain<-prec.array[lon.gutu,lat.gutu,]

plot(time,gutu.rain[1,],main = "Time series for Gutu Rainfall (Jan 1981- Jan 2020)",
     xlab = "Time(Months/Years)",ylab = "Rainfall (in mm/month)",type = 'l')

#For regression, i am using a Simple Linear regression
df.gutu <- as.data.frame(gutu.rain)
regression.fit<-lm(V469 ~ V1 + V2 + V225+V290+V300 + V350, data=df.gutu) #random picked variables
plot(regression.fit)
