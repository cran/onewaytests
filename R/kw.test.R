kw.test <- function(y, group){

  n.i<- Rmean.i<- KW.noties<- ties<-correction<- KW.stat<- p.value<-NULL
  ranks = rank(y)
  N<-length(y)
  x.levels <- levels(factor(group))
  t<-as.data.frame(table(rank(y)))
  ties<-t$Freq
  for (i in x.levels) {
    n.i[i] <- length(y[group==i])
    Rmean.i[i]<-mean(ranks[group==i])
  }
  

  KW.noties = 12/(N*(N+1)) * sum( n.i*(Rmean.i - (N+1)/2)^2 )

  correction <- (1 - sum( ties^3 - ties )/(N^3 - N) )
  KW.stat<- KW.noties/correction
  p.value<-pchisq(KW.stat, df=(length(x.levels)-1),lower.tail = FALSE)
  return(list(statistic = KW.stat, df= (length(x.levels)-1), p.value=p.value))
}