dmi.df <- read.csv("dmi.csv")
dim(dmi.df)

#Convert to TS data in proper frame
temp <- ts(dmi.df[,2],start=c(1982,1),freq=12)
# Plotting Time Plots
ts.plot(temp,ylab="Absolute DMI")
