kw.test <- function(y, group, alpha = 0.05, na.rm = TRUE, verbose = TRUE){

  dname1 <- deparse(substitute(y))
  dname2 <- deparse(substitute(group))
  DNAME <- paste(dname1, "and", dname2)
  METHOD <- "Kruskal-Wallis Test"

 if (na.rm){
    completeObs <- complete.cases(y, group)
    y <- y[completeObs]
    group <- group[completeObs]
  }

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
  approx<- KW.noties/correction
  p.value<-pchisq(approx, df=(length(x.levels)-1),lower.tail = FALSE)
  df=(length(x.levels)-1)


if (verbose) {
            cat("\n", "",METHOD, "\n", 
                sep = " ")
            cat("---------------------------------------------------------", 
                "\n", sep = " ")
            cat("  data :", DNAME, "\n\n", sep = " ")
            cat("  statistic  :", approx, "\n", sep = " ")
            cat("  parameter  :", df, "\n", sep = " ")
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
result$statistic <- approx
result$parameter <- df
result$p.value <- p.value
result$method <- METHOD 
result$y <- y
result$group <- group

attr(result, "class") <- "owt"
invisible(result)

}