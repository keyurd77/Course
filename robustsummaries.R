set.seed(1)
x=c(rnorm(100,0,1)) ##real distribution
x[23] <- 100 ##mistake made in 23th measurement
boxplot(x)
mean(x)
sd(x)
median(x)
mad(x)
set.seed(1)
x=c(rnorm(100,0,1)) ##real distribution
x[23] <- 100 ##mistake made in 23th measurement
y=c(rnorm(100,0,1)) ##real distribution
y[23] <- 84 ##similar mistake made in 23th measurement
par(mfrow=c(1,1))
plot(x,y,main=paste0("correlation=",round(cor(x,y),3)),pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
abline(0,1)
par(mfrow=c(1,2))
plot(x,y,main=paste0("correlation=",round(cor(x,y),3)),pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
plot(rank(x),rank(y),main=paste0("correlation=",round(cor(x,y,method="spearman"),3)),pch=21,bg=1,xlim=c(-3,100),ylim=c(-3,100))
abline(0,1)
# if we know there are outliers then median and mad are recommended over 
#the mean and standard devtiaion conterparts this are robust but robust are less powerful than non-robust
x <- 2^(-5:5) ##this 1/32,1/16,1/8,...,1,2,...,32
par(mfrow=c(1,2))
plot(x)
abline(h=1)
plot(log2(x))
abline(h=0)#so log ratios are more symmetric around 0 than non-log ratios
x=2^seq(1,5)
y=c(rev(1/x),1,x)
Names=c(paste0("1/",rev(x)),1,x)
par(mfrow=c(1,1))
plot(seq(along=y),y,xlab="",ylab="",type="n",xaxt="n")
text(seq(along=y),y,Names,cex=1.5)
abline(h=1)
plot(seq(along=y),y,xlab="",ylab="",type="n",log="y",xaxt="n")
text(seq(along=y),y,Names,cex=1.5)
abline(h=1)

data(ChickWeight)
plot(ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
#using reshape covert the data from 'long format'to 'wide format'
# the plotting library ggplot2 and the manipulation library dplyr want to have data in the long format
head(ChickWeight)
chick <- reshape(ChickWeight,idvar=c("Chick","Diet"),timevar="Time",direction="wide")
head(chick)
#The meaning of this line is: reshape the data from long to wide, where the columns Chick 
#and Diet are the ID's and the column Time indicates different observations for each ID.
#The only remaining step is that we want to remove any chicks which have missing observations
#at any time points (NA for "not available") . The following line of code identifies these rows, and then removes them
chick <-na.omit(chick)
mean(chick$weight.4)
chickoutlier<-c(chick$weight.4, 3000)
mean(chickoutlier)
mean(chickoutlier)/mean(chick$weight.4)#2.062407
median(chickoutlier)/median(chick$weight.4)#1
sd(chickoutlier)/sd(chick$weight.4)#101.2859
mad(chickoutlier)/mad(chick$weight.4)#1
chickoutlier21<-c(chick$weight.21, 3000)
par(mfrow=c(2,2))
plot(chick$weight.4, chick$weight.21, main=paste0("correlation=",round(cor(chick$weight.4, chick$weight.21),3)))
plot(chickoutlier, chickoutlier21, main=paste0("correlation=",round(cor(chickoutlier, chickoutlier21),3)))
cor(chickoutlier, chickoutlier21)/cor(chick$weight.4, chick$weight.21)#pearson correlation more affected by outliers
plot(chick$weight.4, chick$weight.21, main=paste0("correlation=",round(cor(chick$weight.4, chick$weight.21,method="spearman"),3)))
plot(chickoutlier, chickoutlier21, main=paste0("correlation=",round(cor(chickoutlier, chickoutlier21,method="spearman"),3)))
cor(chickoutlier, chickoutlier21,method="spearman")/cor(chick$weight.4, chick$weight.21,method="spearman")#spearman not so much
