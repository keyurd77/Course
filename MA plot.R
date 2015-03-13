source("http://bioconductor.org/biocLite.R")
biocLite("Biobase")
biocLite("SpikeInSubset")
library(SpikeInSubset)
data(mas95)
par(mfrow=c(1,2))
r <- exprs(mas95)[,1] ##original measures were not logged
g <- exprs(mas95)[,2]
plot(r,g,lwd=2,cex=0.2,pch=16,
     xlab=expression(paste(E[1])),
     ylab=expression(paste(E[2])), 
     main=paste0("corr=",signif(cor(r,g),3)))
abline(0,1,col=2,lwd=2)
f <- function(a,x,y,p=0.95) mean(x<=a & y<=a)-p
a95 <- uniroot(f,lower=2000,upper=20000,x=r,y=g)$root
abline(a95,-1,lwd=2,col=1)
text(8500,0,"95% of data below this line",col=1,cex=1.2,adj=c(0,0))
r <- log2(r)
g <- log2(g)
plot(r,g,lwd=2,cex=0.2,pch=16,
     xlab=expression(paste(log[2], " ", E[1])),
     ylab=expression(paste(log[2], " ", E[2])),
     main=paste0("corr=",signif(cor(r,g),3)))
abline(0,1,col=2,lwd=2)#scatterplot
#MAPlot
par(mfrow=c(1,1))
plot((r+g)/2,(r-g),lwd=2,cex=0.2,pch=16,
     xlab=expression(paste("Ave{ ",log[2], " ", E[1],", ",log[2], " ", E[2]," }")),
     ylab=expression(paste(log[2]," { ",E[1]," / ",E[2]," }")),
     main=paste0("SD=",signif(sqrt(mean((r-g)^2)),3)))
abline(h=0,col=2,lwd=2)
set.seed(12201970)
before <- runif(6, 5, 8)
after <- rnorm(6, before*1.05, 2)
li <- range(c(before, after))
ymx <- max(abs(after-before))
par(mfrow=c(1,2))
plot(before, after, xlab="Before", ylab="After",
     ylim=li, xlim=li)
abline(0,1, lty=2, col=1)


plot(before, after-before, xlab="Before", ylim=c(-ymx, ymx),
     ylab="Change (After - Before)", lwd=2)
abline(h=0, lty=2, col=1)

filename <- "fig8dat.csv"
url <- "https://github.com/kbroman/Talk_Graphs/raw/master/R/fig8dat.csv"
if (!file.exists(filename)) download(url, filename)
x <- read.table(filename, sep=",", header=TRUE)
plot(x[,1],x[,2],xlab="log Dose",ylab="Proportion survived",ylim=c(0,1),
     type="l",lwd=2,col=1)
lines(x[,1],x[,3],lwd=2,col=2)
lines(x[,1],x[,4],lwd=2,col=3)
legend(1,0.4,c("Drug A","Drug B","Drug C"),lwd=2, col=1:3)

