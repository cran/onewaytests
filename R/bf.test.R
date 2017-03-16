bf.test <- function (y, group, alpha = 0.05, na.rm = TRUE, verbose = TRUE){
  
  dname1 <- deparse(substitute(y))
  dname2 <- deparse(substitute(group))
  DNAME <- paste(dname1, "and", dname2)
  METHOD <- "Brown-Forsythe Test"


if (na.rm){
    completeObs <- complete.cases(y, group)
    y <- y[completeObs]
    group <- group[completeObs]
  }

  n <- length(y)
  x.levels <- levels(factor(group))
  y.vars <- y.means <- m <- y.n <- NULL
  y.mean = mean(y)
  for (i in x.levels) {
    y.vars[i] <- var(y[group == i])
    y.means[i] <- mean(y[group == i])
    y.n[i] <- length(y[group == i])
  }
  for (j in x.levels) {
    m[j] <- (1 - y.n[j]/n) * (y.vars[j])/sum((1 - y.n/n) * 
                                               (y.vars))
  }
  SSb = sum(y.n * ((y.means - y.mean)^2))
  denom = sum((1 - y.n/n) * (y.vars))
  Ftest = SSb/denom
  df1 = length(x.levels) - 1
  df2 = 1/(sum(m^2/(y.n - 1)))
  p.value = pf(Ftest, df1, df2, lower.tail = F)
  
  
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