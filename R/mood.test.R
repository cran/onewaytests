mood.test <- function(y, group) {
  
  y.median <-Fisher<-Chisquare<-Chisquare_pvalue<-Chisquare_df<-NULL
  
  y.median=median(y)
 
    Chisquare<-chisq.test(y < y.median, group)$statistic
    Chisquare_df<-chisq.test(y < y.median, group)$parameter
    Chisquare_pvalue<-chisq.test(y < y.median, group)$p.value
    return(list(statistic=Chisquare, df=Chisquare_df, p.value=Chisquare_pvalue))
  
  
}
