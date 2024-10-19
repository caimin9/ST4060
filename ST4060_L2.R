cars
plot(cars, pch=20, cex=3)
lmo = lm(dist~speed, data=cars)
abline(lmo, lwd=5, col=3)

# (see LearnR pages from the 
# "General Resouces" page on Canvas
# for more)