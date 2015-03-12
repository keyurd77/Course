set.seed(1)#Each time a random number table is created, the Random Number Generator will produce the 
#same set of random numbers, until the Seed value is changed.
dat<-read.csv("mice_pheno.csv")
hfpop<-dat[dat$Sex=="F" & dat$Diet=="hf",3]
chowpop<-dat[dat$Sex=="F" & dat$Diet=="chow",3]
N<-5
hf<-sample(hfpop,N)
chow<- sample(chowpop,N)
t.test(hf, chow)
#p-value = 0.141, so not rejecting the null, but we know that there is a difference
#but because we do not have enough samples we do not have the power.
#Type I error = when we reject the null when we should have not rejected the null (false positive)
#there is no change but we find change
#Type II error = where we know the null's not true and we didn't reject it (false negative)
#there is change but we do not find change
N<-12
alpha <- 0.05 # where we reject null
B<- 10000
rejections <- sapply(1:B, function(i){
  hf<-sample(hfpop, N)
  chow<-sample(chowpop, N)
  t.test(hf, chow)$p.value< alpha
}) # taking 10000 times, sample of size 12,  and doing this 10000 times and then
#then taking a t.test to see how many times we reject null as p.value<alpha
#so rejection is a vector of true and false
head(rejections)
mean(rejections)
# 0.2254 is the power so we have 22% power with N samples
Ns<-seq(5, 50, 5)
alpha <- 0.05 # where we reject null
B<- 10000
power <- sapply(Ns, function(N){
  rejections<-sapply(1:B, function(i){
  hf<-sample(hfpop, N)
  chow<-sample(chowpop, N)
  t.test(hf, chow)$p.value< alpha
})
return(mean(rejections))
})
plot(Ns, power) # as more N more power


