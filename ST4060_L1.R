dat = read.csv("insdata.csv")
dat
dim(dat)
head(dat)
plot(dat[,1],log(dat[,2]))

dat[,1]
dat[,2]
dat[3,]

n = nrow(dat)
for(i in 1:n){
	print(dat[i,2])
}