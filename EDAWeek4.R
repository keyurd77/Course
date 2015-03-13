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
#SCATTERPLOT
data("father.son")
x=father.son$fheight
y=father.son$sheight
plot(x,y,xlab="Father's height in inches",ylab="Son's height in inches",main=paste("correlation =",signif(cor(x,y),2)))
groups <- split(y,round(x)) 
groups
boxplot(groups)
print(mean(y[ round(x) == 72]))
x=(x-mean(x))/sd(x)
y=(y-mean(y))/sd(y)
means=tapply(y,round(x*4)/4,mean)
fatherheights=as.numeric(names(means))
mypar2(1,1)
plot(fatherheights,means,ylab="average of strata of son heights",ylim=range(fatherheights))
abline(0,cor(x,y))
a=rnorm(100);a[1]=10
b=rnorm(100);b[1]=11
plot(a,b,main=paste("correlation =",signif(cor(a,b),2)))
plot(father.son$fheight, father.son$sheight, main=paste("cor=",signif(cor(father.son$fheight, father.son$sheight),3)))
identify(father.son$fheight, father.son$sheight)
#click on the graph after this and hit 'esc' on the keyboard to get row number for that point
n = nrow(father.son)
plot(scale(x), scale(y))# scale subtracts the mean and divides by the standard deviation
abline(h=0, v=0)
mean(scale(x)*scale(y))#0.5008732
cor(x,y)
sum(scale(x) * scale(y)) / (n - 1)
data(nym.2002)
head(nym.2002)
hist(nym.2002$time)
plot(nym.2002$age, nym.2002$time)
plot(nym.2002$time, nym.2002$place)
qqnorm(nym.2002$time)
qqline(nym.2002$time)
barplot(tail(sort(table(nym.2002$home)),10))
boxplot(nym.2002$time~nym.2002$gender)
time = sort(nym.2002$time)
head(time)
median(time)
time[1]/median(time)
min(time) / median(time) # 0.5605402
max(time)/median(time)#2.156368
plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))
plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)
library("downloader")
filename <- "fig1.RData"
url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig1.RData"
if (!file.exists(filename)) download(url,filename)
load(filename)
par(mfrow=c(1,1))
dat <- list(Treatment=x,Control=y)
boxplot(dat,xlab="Group",ylab="Response",xlab="Group",ylab="Response",cex=0)
stripchart(dat,vertical=TRUE,method="jitter",pch=16,add=TRUE,col=1)
library(downloader)
url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig3.RData"
filename <- "fig3.RData"
if (!file.exists(filename)) download(url, filename)
load(filename)
par(mfrow=c(1,2))
dat <- list(Treatment=x,Control=y)
boxplot(dat,xlab="Group",ylab="Response",xlab="Group",ylab="Response",cex=0)
stripchart(dat,vertical=TRUE,method="jitter",pch=16,add=TRUE,col=1)
boxplot(dat,xlab="Group",ylab="Response",xlab="Group",ylab="Response",log="y",cex=0)
stripchart(dat,vertical=TRUE,method="jitter",pch=16,add=TRUE,col=1)

