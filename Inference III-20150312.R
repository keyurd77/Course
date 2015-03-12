install.packages("downloader")
library(downloader)
url<-"https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- tempfile()
set.seed(1)
download(url,destfile="babies.txt")
dat <- read.table("babies.txt",header=TRUE)
smokers <- sample(dat$bwt[dat$smoke==1],10)
nonsmokers <- sample(dat$bwt[dat$smoke==0],10)
mean(smokers)-mean(nonsmokers)
#[1] -2.6
for(i in 1:10) {
  smokers <- sample(dat$bwt[dat$smoke==1],10)
  nonsmokers <- sample(dat$bwt[dat$smoke==0],10)
  cat("observed difference = ",mean(smokers)-mean(nonsmokers),"ounces\n")
}# draw random 10 samples
ttestgenerator <- function(n) {
  # note that here we have a false "smokers" group where we actually
  # sample from the nonsmokers. this is because we are modeling the *null*
  smokers = sample(dat$bwt[dat$smoke==0], n)
  nonsmokers = sample(dat$bwt[dat$smoke==0], n)
  return((mean(smokers)-mean(nonsmokers))/sqrt(var(smokers)/n + var(nonsmokers)/n))
}#making a new function for generating tests
ttests <- replicate(1000, ttestgenerator(10))#repeat this function 1000 times with 10 ns
hist(ttests)
qqnorm(ttests)
abline(0,1)
ttests <- replicate(1000, ttestgenerator(3)) # do the same for n of 3
qqnorm(ttests)
abline(0,1)# now the tails are more broader
qs <- (seq(0,999)+0.5)/1000
qqplot(qt(qs,df=2*3-2),ttests,xlim=c(-6,6),ylim=c(-6,6))
abline(0,1)
qqnorm(dat$bwt[dat$smoke==0])
qqline(dat$bwt[dat$smoke==0])
nonsmokerweights <- rnorm(5000, 
                          mean=mean(dat$bwt[dat$smoke==0]), 
                          sd=sd(dat$bwt[dat$smoke==0]))

bwt.nonsmoke = babies$bwt[babies$smoke==0]
pop.var = var(bwt.nonsmoke)
sam.var <- replicate(1000, var(sample(bwt.nonsmoke, 10)))
hist(sam.var, breaks=100)
abline(v=pop.var, col="Red")
mean(sam.var>1.5*pop.var)
#0.179
sam.var <- replicate(1000, var(sample(bwt.nonsmoke, 50)))
hist(sam.var, breaks=100)
abline(v=pop.var, col="Red")
mean(sam.var>1.5*pop.var)
#0.032
sample.size = 2:400
var.estimate = sapply(sample.size, function(n) var(sample(bwt.nonsmoke, n)))
plot(sample.size, var.estimate)
abline(h=pop.var, col="blue")
