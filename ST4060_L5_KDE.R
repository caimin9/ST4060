# Example of KDE

# sample
x = cars$dist
hist(x)

# density estimation
fh = density(x)
names(fh)
plot(fh)
rug(x)

# overlay on top of the histogram
hist(x, freq=FALSE)
points(fh, lwd=2)
fh1 = density(x, bw=5) # reducing h
points(fh1, lwd=2, col=4)
fh2 = density(x, bw=15) # increasing h
points(fh2, lwd=2, col=2)

# Q1.1

# Simulate n=1000 realisations X ~ Exp(0.5)
N = 1000
set.seed(4060) # to get the same sample every time
x = rexp(n=N, rate=.5)
hist(x, freq=FALSE)

# Fit a KDE to the sample
fh = density(x, bw=1)

# Overlay density estimate
points(fh, lwd=5, t='l')


