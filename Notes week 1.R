x <- rnorm(n=100, mean=3, sd=1)
hist(x, prob=T)
lines(density(x))
