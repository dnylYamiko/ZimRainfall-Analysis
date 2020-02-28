setwd("~Zimbabwe_Rain_Analysis_1981-2020")
##This script is analysing data from 
##https://iridl:ldeo.columbia.edu/SOURCES/UCSB/CHIRPS/v2p0/

require(fields)
require(ncdf4)
require(raster)
require(zoo)
require(xts)
require(ggplot2)
require(chron)
require(lattice)
require(RColorBrewer)
require(cluster)

rain<-"precipitation"
dat = nc_open("data2.nc")

print(dat)

#the data has 3 dimensions, precipitation[X,Y,T].
zim_long = ncvar_get(dat,"X")
zim_lat = ncvar_get(dat,"Y")
tm = ncvar_get(dat,"T")

rain.array <-ncvar_get(dat,rain)
long_name <- ncatt_get(dat,rain,"long_name")
rain.units <- ncatt_get(dat,rain,"units")
miss_value <-ncatt_get(dat,rain,"missing_value")
sd.name <- ncatt_get(dat,rain,"standard_name")

nc_close(dat)

cat("The mean rainfall is ", mean(rain.array,na.rm = TRUE))
cat("\nThe standard deviation is ", sd(rain.array, na.rm = TRUE))
cat("\nThe maximum rainfall is ", max(rain.array))
cat("\nThe minimum rainfall is ", min(rain.array))

tm<-seq(as.Date("1981-01-01"),to=as.Date('2020-01-01'),"months")
tm<- as.yearmon(time)

rain.array[rain.array==miss_value$value] <- NA
head(as.vector(rain.array[,,1]))
length(na.omit(as.vector(rain.array[,,1])))

#for the first year 
rain.slice<-rain.array[,,1]

image.plot(zim_long,zim_lat,rain.slice)
map(add = T)

#creating a grid of (longitude,latitude) pairs over the map
lonlat <- as.matrix(expand.grid(zim_long,zim_lat))
rain.vec<-as.vector(rain.slice)

#rainfall for the first slice
rain01.df <- data.frame(cbind(lonlat,rain.vec))
names(rain01.df)<-c("longitude","latitude",paste(rain,as.character(1981),sep = "_"))
print(head(na.omit(rain01.df),10))

rain.vecAll<-as.vector(rain.array)
rain.mat <- matrix(rain.vecAll,nrow = length(zim_long)*length(zim_lat),ncol = length(tm))

#This creates a dataframe for the entire rainfall brick (zim_lon X zim_lat x time)
rain.All <- data.frame(cbind(lonlat,rain.mat))
names(rain.All)<-c("longitude","latitude",as.character(tm))
print(head(na.omit(rain.All),2))

rain.All$Mean = apply(rain.All[3:471],1,mean)
rain.All$StDeviation = apply(rain.All[3:471],1,sd)
rain.All$Maximun = apply(rain.All[3:471],1,max)
rain.All$Minimun = apply(rain.All[3:471],1,min)

print(head(na.omit(rain.All),2))

# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(rain.All[,3:471], 3, nstart = 25)
print(km.res)
clusplot(rain.All[,3:471],km.res$cluster,color = TRUE, shade = TRUE)
