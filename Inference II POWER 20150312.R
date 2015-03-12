babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]
#effect size
mean(bwt.nonsmoke) - mean(bwt.smoke)# because this is the whole population we know that
#the true effect size or diff is as below
#8.937666, Below is the code that I ran and gave the right answer
#N<- 15
#alpha0.1 <-0.1
#alpha0.05<-0.05
#alpha0.01<-0.01
#B<- 1000
#smoking<- sample(bwt.smoke, N)
#nonsmoking<-sample(bwt.nonsmoke, N)
#rejections<-t.test(smoking, nonsmoking)$p.value<alpha
#B<- 1000
#rejections<- sapply(1:B, function(i){
  #smoking<- sample(bwt.smoke, N)
  #nonsmoking<-sample(bwt.nonsmoke, N)
#t.test(smoking, nonsmoking)$p.value<alpha0.1  
#})
#mean(rejections) 0.395
run_experiment = function(N, alpha) {
  
  dat.ns = sample(bwt.nonsmoke, N)
  
  dat.s = sample(bwt.smoke, N)
  
  t.test(dat.ns, dat.s)$p.value < alpha
  
}

mean(replicate(1000, run_experiment(15, .1)))#0.377
mean(replicate(1000, run_experiment(15, .05)))# 0.266
mean(replicate(1000, run_experiment(15, .01)))#0.104
#20 students
#If 1% of the time, the value a student gets from executing 
#the correct code is outside the interval, and 2,000 students 
#perform this (each one getting a different random value), 
#then .01 times 2,000 = 20 students will be marked wrong incorrectly.

       



