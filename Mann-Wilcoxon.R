set.seed(779) ##for illustration
N=25
x<- rnorm(N,0,1)
y<- rnorm(N,0,1)
x[1]<-5;x[2]<-7#changed on value to an outlier and normal t-test gives a lower p-value it is affected
cat("t-test pval:",t.test(x,y)$p.value)
cat("Wilcox test pval:",wilcox.test(x,y)$p.value)#whereas wilcox test is more robust
par(mfrow=c(1,1))
stripchart(list(x,y),vertical=TRUE,ylim=c(-7,7),ylab="Observations",pch=21,bg=11,cex=1.25)
abline(h=0)
xrank<-rank(c(x,y))[seq(along=x)]
yrank<-rank(c(x,y))[-seq(along=y)]
stripchart(list(xrank,yrank),vertical=TRUE,ylab="Ranks",pch=21,bg=11,cex=1.25)
ws <- sapply(x,function(z) rank(c(z,y))[1]-1)
text(rep(1.05,length(ws)),xrank,ws)
W <-sum(ws) 
n1<-length(x);n2<-length(y)
Z <- (mean(ws)-n2/2)/ sqrt(n2*(n1+n2+1)/12/n1)
print(Z)

data(ChickWeight)
chick <- reshape(ChickWeight,idvar=c("Chick","Diet"),timevar="Time",direction="wide")
chick <- na.omit(chick)
stripchart(chick$weight.4 ~ chick$Diet, method="jitter", vertical=TRUE)

x<-chick$weight.4 [chick$Diet==1]
y<-chick$weight.4 [chick$Diet==4]
t.test(x,y)
wilcox.test(x,y)
x1<-c(x, 200)
t.test(x1,y)$p.value
wilcox.test(x1,y)$p.value
par(mfrow=c(1,3))
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)
t.test(x,y+10)$statistic - t.test(x,y+100)$statistic
