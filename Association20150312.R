tab<-matrix(c(180, 40, 20, 20), 2,2)
tab
rownames(tab) <- c("AA or Aa", "aa")
colnames(tab) <- c("Controls", "Cases")
prop.table(tab)
tab[2,2]<-10
tab
prop.table(tab,1)
ctest<-chisq.test(tab)
ctest
d<- read.csv("assoctest.csv")
test<-chisq.test(table(d))
test#3.3437
fisher<-fisher.test(table(d))
fisher
