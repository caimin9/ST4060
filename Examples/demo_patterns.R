# Kaggle dataset:
# https://www.kaggle.com/datasets/saurabhshahane/electricity-load-forecasting?resource=download

dat = read.csv('continuous dataset.csv')
head(dat)
dat$hour
dat = dat[-which(dat$dayofyear=="29-Feb"),]
hrs = unique(dat$hour)
days = unique(dat$dayofyear)
nd = length(days)
days = days[c(c((nd-1):nd),c(1:(nd-2)))]
sdat = subset(dat,hour==hrs[12])
dim(sdat)
head(sdat)
temp = sdat$T2M_toc
day = sdat$dayofyear
year = sdat$year

par(mfrow=c(2,1), mar=c(4,4,3,1))

plot(temp,pch=20,t='b',main="Temperature",
	font.lab=2,las=2,xlab="day")

is = which(sdat$year=="2015")
# plot(temp[is],ylim=range(sdat$T2M_toc),t="n",
	# main="Temperature",
	# xlab="day of the year",ylab="temp")
df.day = df.temp = NULL
for(yr in unique(year)){
	ssdat = subset(sdat,year==yr)
	temp = ssdat$T2M_toc
	day = ssdat$dayofyear
	id.day = match(day,days)
	# points(id.day,temp,pch=20,col=8)
	df.day = c(df.day,id.day)
	df.temp = c(df.temp, temp)
}

# model 1:
sp = smooth.spline(df.day,df.temp)
# model 2:
require(splines)
xm = bs(df.day)
pp = lm(df.temp~xm)
# plot to compare:
plot(df.day,df.temp,pch=20,col=8,
	xlab="Day of year",ylab="Temperature",
	main="Which model is better?")
lines(sp,lwd=6,col=2)
is = order(df.day)
lines(df.day[is],fitted(pp)[is],lwd=6,col=4)
legend("topleft",bty='n',lwd=3,col=c(2,4),
	legend=c("model 1","model 2"))
	
