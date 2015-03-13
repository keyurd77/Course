install.packages("UsingR")
library(UsingR)
x=father.son$fheight
round(sample(x,20),1)
hist(x)
bins <- seq(floor(min(x)),ceiling(max(x)))
hist(x,breaks=bins,xlab="Height",main="Adult men heights")
myCDF<-ecdf(x)
xs<-seq(floor(min(x)),ceiling(max(x)),0.1) 
plot(xs,myCDF(xs),type="l",xlab="x=Height",ylab="F(x)")
mu <- mean(x)
popsd <- function(x) sqrt(mean((x-mean(x))^2)) 
popsd(x)
1-pnorm(72,mean(x),popsd(x)) 
ps <- seq(0.01,0.99,0.01)
qs <- quantile(x,ps)
normalqs <- qnorm(ps,mean(x),popsd(x))
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height percentiles")
abline(0,1)
qqnorm(x)
qqline(x) 
n <-1000
x <- rnorm(n)
qqnorm(x)
qqline(x)
dfs <- c(3,6,12,30)
par(mfcol=c(1,1))
for(df in dfs){
  x <- rt(1000,df)
  qqnorm(x,xlab="t quantiles",main=paste0("d.f=",df),ylim=c(-6,6))
  qqline(x)
}
hist(exec.pay)
qqnorm(exec.pay)
qqline(exec.pay)
boxplot(exec.pay,ylab="10,000s of dollars",ylim=c(0,400))

load("skew.RData")
dim(dat)
par(mfcol=c(1,1))
for (i in 1:9) {
  hist(dat[,])
}
hist(dat[,9])
hist(dat[,4])

dat1<-data.frame(dat)
for ( i in seq(1,length(dat1),1) ) hist(dat1[,i],ylab=names(dat1[i]),type="l")

head(InsectSprays)
View(InsectSprays)
str(InsectSprays)
boxplot(split(InsectSprays$count, InsectSprays$spray))
boxplot(InsectSprays$count~InsectSprays$spray)
#C

