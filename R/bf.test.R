bf.test <- function(y, group) {
  
  n <- length(y)
  x.levels <- levels(factor(group))
  y.vars <- y.means <- m <- y.n <- NULL
  
  y.mean=mean(y)
  
  for (i in x.levels) {
    
    y.vars[i] <- var(y[group==i])
    
    y.means[i] <- mean(y[group==i])
    
    y.n[i] <- length(y[group==i])
    
  }
  
  for (j in x.levels) {
    
    m[j] <- (1-y.n[j]/n)*(y.vars[j])/sum((1-y.n/n)*(y.vars))
    
  }
  
  
  SSb=sum(y.n*((y.means-y.mean)^2))
  
  denom=sum((1-y.n/n)*(y.vars))
  
  Ftest=SSb/denom
  
  df1=length(x.levels)-1
  df2=1/(sum(m^2/(y.n-1)))
  
  
  
  p.value=pf(Ftest,df1,df2,lower.tail = F)
  
  
  return(list(statistic = Ftest, df1 = df1, df2 = df2, p.value = p.value))
  
}


