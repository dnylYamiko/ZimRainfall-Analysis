setwd("~Zimbabwe_Rain_Analysis_1981-2020")
library(cluster)
library(HSAUR)
require(xts)

df<-read.table("dmi_long_data.txt",header = FALSE)

cols <- c("Year","Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec")
names(df)<-cols

df$Year <- as.Date(as.character(df$Year), format = "%Y")

#Calculating mean, standard deviation, max and min: per month and per year.

mean_month <-apply(df[,-1],2,mean)
sd_month <-apply(df[,-1],2,sd)
min_month <-apply(df[,-1],2,min)
max_month <-apply(df[,-1],2,max)

df$mean_year <-apply(df[,-1],1,mean)
df$sd_year <-apply(df[,-1],1,sd)
df$min_year <-apply(df[,-1],1,min)
df$max_year <-apply(df[,-1],1,max)

print(head(df,5))

plot(df$Year,max_year,type = 'l',xlab = "Years",ylab = "Dipole Index",col = "blue",pch="o",
     lty = 1
     )
lines(df$Year,min_year,col="red",pch="*",lty = 2)
lines(df$Year,sd_year,col="black",pch="*",lty = 3)
lines(df$Year,mean_year,col="green",pch="x",lty = 4)
legend("topleft",legend=c("max per year","min per year","sd per year","mean per year"),
       col=c("blue","red","black","green"),
       pch=c("o","*","+","x"),lty=c(1,2,3,4), ncol=1)

# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df[,-1], 3, nstart = 25)
print(km.res)
clusplot(df,km.res$cluster,color = TRUE, shade = TRUE)

#The following code is plotting a time-series uses the other dmi data i.e. dmi.csv
##taken from http://www.jamstec.go.jp/virtualearth/general/en/index.html
##Zimbabwe coordinates were used to extract the data
dmi.df <- read.csv("dmi.csv")
dim(dmi.df)

#Convert to TS data,
temp <- ts(dmi.df[,2],start=c(1982,1),freq=12)
# Plotting Time Plots
ts.plot(temp,ylab="Absolute DMI")
