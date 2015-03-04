dat <- read.csv("mice_pheno.csv")# entire population data
pop <- dat[dat$Sex=="F" & dat$Diet=="chow", 3]# only females on control diet and their bodyweight
mu <- mean(pop)# get the mean of the population
#[1] 23.89338
#but usually we do not have the population mean what we have is a sample
N<-30 # say size 30
y<- sample(pop, N) # so sample size of 30 randomly from the population
mean(y) # this is a random variable
#[1] 24.18567
se<- sd(y)/sqrt(N) # so the standard error is this
se
#[1] 0.6233806, so mean(y) can deviate by so much, 24.18.. +- 0.6233
# so how to create a confidence interval such that within this interval
# the population mean (which we do not have) will definitely fall, we are going to use the CLT
# theorem here and knowledge of normal distribution
#mean(pop) - mean(y) we know that this is going to be 0 and in normal distribution sd = 1
# so mu of 0 and sd of 1, so we want confidence interval Q such that there is a 95% prob
# that the mean of pop with be in the interval from both sides (2)
Q <- qnorm(1-0.05/2) # so the quantile
Q # [1] 1.959964, so 95% of the times a normal random variable falls between 1.95 and -1.95
#probability of this happening is 95%, but we can't have mean (pop) as we don't know
# -Q < mean(pop) - mean(y)/ se < +Q, so instead we have to multiply se to both sides and add mean of y
interval<- c(mean(y) - Q*se, mean(y)+Q*se)
interval # [1] 22.96386 25.40747
plot (mu + c(-7,7), c(1,1), type ="n", xlab ="Weights",
      ylab="Intervals", ylim = c(1,100)) # plotting mu, x axis range -7 to 7 of mu to cover the 
#confidence interval, type 'n' does not plot straightaway, rest is intuitive
abline(v=mean(pop))# draw a line vertical (v), h for horisontal, at mean of the pop
lines(interval, c(1,1)) # draw a line where the interval falls, interval is a random variable
# as it is the mean of y which is a random variable. we will take 100 of these and plot
for(i in 2:100){
  y<- sample(pop, N)#same values as above in a for loop
  se<- sd(y)/sqrt(N)
  interval<- c(mean(y) - Q*se, mean(y)+Q*se)
  colour <-ifelse(interval[1]<=mean(pop) &
                    interval[2] >=mean(pop), 1,2)# colour if one interval falls either negative to
  #mean pop or positive to mean pop
  lines(interval, c(i,i), col=colour)# draw lines and col with colour for outliers
}# everytime you do it about 5% should not fall on the mean as it is 95% confidence interval
# say our N size is 5
N<- 5
for(i in 1:100){
  y<- sample(pop, N)#same values as above in a for loop
  se<- sd(y)/sqrt(N)
  interval<- c(mean(y) - Q*se, mean(y)+Q*se)
  colour <-ifelse(interval[1]<=mean(pop) &
                    interval[2] >=mean(pop), 1,2)# colour if one interval falls either negative to
  #mean pop or positive to mean pop
  lines(interval, c(i,i), col=colour)# draw lines and col with colour for outliers
}  # we start seeing broader confidence intervals as expected because sample size has decreased
#but also outliers have increased more than 5% which means that CLT is not working as the 
#sample size is less than 30. the part in the above code which contains the CLT is the 'Q'
#definition and we would have to change that to quantile of t-statistics
Q<- qt(1-0.05/2,4) # and we have to give it degrees of freedom which is sample size -1
# here N-1 (5-1 =4)

N<- 5
y<- sample(pop, N)
se<- sd(y)/sqrt(N)
Q<- qt(1-0.05/2,4)
interval<- c(mean(y) - Q*se, mean(y)+Q*se)
plot (mu + c(-7,7), c(1,1), type ="n", xlab ="Weights",
      ylab="Intervals", ylim = c(1,100))
abline(v=mean(pop))
for(i in 1:100){
  y<- sample(pop, N)
  se<- sd(y)/sqrt(N)
  interval<- c(mean(y) - Q*se, mean(y)+Q*se)
  colour <-ifelse(interval[1]<=mean(pop) &
                    interval[2] >=mean(pop), 1,2)
  lines(interval, c(i,i), col=colour)
}# so now after changing to t-distribution rather than normal you will see that things
# are back to within 5%, to quickly see how confidence interval relates to p value
dat <- read.csv ("femaleMiceWeights.csv")
t.test(dat[13:24,2], dat[1:12,2])
# so mean of population and sample should be zero (which is the null distribution)
#we are checking that by p value then a confidence interval which includes 0
# as here (-0.04 to 6.08) the p value would be greater than 0.05 in favor of null distribution
# as all the interval lines are going to fall 95% of the times on 0 and if it does not include 0 then p
#value is lower than 0.05 so in rejection of the null hypothesis
