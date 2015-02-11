dat <- read.csv("femaleMiceWeights.csv")
dat
dat [1:12, 2]
dat [13:24, 2]
mean(dat[13:24,2]) - mean(dat[1:12,2])
s = split(dat[,2], dat[,1])
s
stripchart(s, vertical=TRUE, col=1:2)
abline(h=sapply(s, mean), col=1:2)
highfat <- s[["hf"]]
highfat
sample(highfat, 6)
sample(highfat, 6, replace=TRUE)
highfat>30
as.numeric(highfat>30)
sum(highfat>30)
mean(highfat>30)
population <- read.csv("femaleControlsPopulation.csv")
population
control <- sample(population[ ,1], 12)
mean(control)

# to perform test 10000 times to see if it is possible to see 3.020833 by random chance
diff <- mean(dat[13:24,2]) - mean(dat[1:12,2])
n<- 10000
null <- vector("numeric", n)
for (i in 1:n){
  control <-sample(population[ ,1], 12)
  treatment <-sample(population[ ,1], 12)
  null[i] <- mean(treatment) - mean(control)
}
mean(null>diff) # what proportion of times where this happens
#only 1 or 2%, this is the p value
population
population = population [,1]
mean (population)

