#http://www.quora.com/How-would-linear-regression-be-described-and-explained-in-laymans-terms
#http://www.stat.ufl.edu/~winner/qmb3250/notespart2.pdf
g <- 9.8 ## meters per second #equation for falling objects d=h0+v0t−0.5×9.8t2
n <- 25
tt <- seq(0,3.4,len=n) ##time in secs, t is a base function
f <- 56.67  - 0.5*g*tt^2
y <-  f + rnorm(n,sd=1)
plot(tt,y,ylab="Distance in meters",xlab="Time in seconds")
lines(tt,f,col=2)
tt2 <-tt^2
fit <- lm(y~tt+tt2) # function in R to fit the data points and provides LSE (least squares estimate)
summary(fit)$coef 
