load("skew.RData")
dim(dat)
par(mfcol=c(1,1))
for (i in 1:9) {
  qqnorm(dat)
}
hist(dat[,9])
