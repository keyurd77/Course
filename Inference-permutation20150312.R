dat <- read.table("babies.txt", header=TRUE)
set.seed(0)
N <- 50
smokers <- sample(dat$bwt[dat$smoke==1],N)
nonsmokers <- sample(dat$bwt[dat$smoke==0],N)
obs <- median(smokers)-median(nonsmokers)
avgdiff <- replicate(1000, {
  all <- sample(c(smokers,nonsmokers))
  smokersstar <- all[1:N]
  nonsmokersstar <- all[(N+1):(2*N)]
  return(median(smokersstar) - median(nonsmokersstar))
})
mean(abs(avgdiff) > abs(obs))
#0

