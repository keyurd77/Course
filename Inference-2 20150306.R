babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- babies$bwt[babies$smoke==0]#extract birthweight of nonsmoker's babies
bwt.smoke<-babies$bwt[babies$smoke==1]# extract " " smoker's babies
mytest<-t.test(bwt.smoke, bwt.nonsmoke)
mytest$p.value
mytest$conf.int
tt<-replicate (1000, sample(bwt.nonsmoke,30) - sample(bwt.smoke, 30))
t.test(tt, alternative = "two.sided", mu=0, conf.level=0.95)
t<-replicate (1000, mean(sample(bwt.nonsmoke,30)) - mean(sample(bwt.smoke, 30)))
t.test(t, alternative = "two.sided", mu=0, conf.level=0.95)
test1000 <- replicate(1000, t.test(sample(bwt.nonsmoke, 30), sample(bwt.smoke, 30))$conf.int)
mean(test1000[2,] - test1000[1,])
#[1] 18.24275 EXPLANATION
CIs = replicate(1000, t.test(sample(bwt.nonsmoke, 30), sample(bwt.smoke, 30))$conf.int)
mean(CIs[2,] - CIs[1,])
popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)
mean(CIs[1,]<=popdiff & popdiff<=CIs [2,])
#[1] 0.957
#Answer mean(CIs[1,] < popdiff & CIs[2,] > popdiff)