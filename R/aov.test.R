aov.test <- function(y, group, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {

  dname1 <- deparse(substitute(y))
  dname2 <- deparse(substitute(group))
  DNAME <- paste(dname1, "and", dname2)
  METHOD <- "One-Way Analysis of Variance"


 if (na.rm){
    completeObs <- complete.cases(y, group)
    y <- y[completeObs]
    group <- group[completeObs]
  }


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

  


if (verbose) {
            cat("\n", "",METHOD, "\n", 
                sep = " ")
            cat("---------------------------------------------------------", 
                "\n", sep = " ")
            cat("  data :", DNAME, "\n\n", sep = " ")
            cat("  statistic  :", Ftest, "\n", sep = " ")
            cat("  num df     :", df1, "\n", sep = " ")
		 cat("  denom df   :", df2, "\n", sep = " ")
		 cat("  p.value    :", p.value, "\n\n", sep = " ")
            cat(if (p.value > alpha) {
                "  Result     : Difference is not statistically significant."
            }
            else {
                "  Result     : Difference is statistically significant."
            }, "\n")
            cat("---------------------------------------------------------", 
                "\n\n", sep = " ")
        }

result <- list()
result$statistic <- Ftest
result$parameter <- c(df1,df2)
result$p.value <- p.value
result$method <- METHOD 
result$y <- y
result$group <- group

attr(result, "class") <- "owt"
invisible(result)


}



