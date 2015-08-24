aov.test <- function(y, group) {

n <- length(y)
x.levels <- levels(factor(group))
y.sums <- y.n <- NULL

sst=sum(y^2)-(sum(y)^2)/n


for (i in x.levels) {

y.sums[i] <- sum(y[group==i])
  
y.n[i] <- length(y[group==i])

}


ssb<- sum(y.sums^2/y.n)-sum(y)^2/n

ssw=sst-ssb 


df1=length(x.levels)-1
df2=n-length(x.levels)


Ftest=ssb/df1*df2/ssw



p.value=pf(Ftest,df1,df2,lower.tail = F)


return(list(statistic = Ftest, df1 = df1, df2 = df2,p.value = p.value))

}


