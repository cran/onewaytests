johansen.test <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {

  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  METHOD <- "Johansen F Test"


 if (na.rm){
    completeObs <- complete.cases(data)
    data <- data[completeObs,]
  }

if (any(colnames(data)==dp[[3L]])==FALSE) stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
if (any(colnames(data)==dp[[2L]])==FALSE) stop("The name of response variable does not match the variable names in the data.")

y = data[[dp[[2L]]]]
group = data[[dp[[3L]]]]


if (!is.factor(group)) stop("The group variable must be a factor.") 
if (!is.numeric(y)) stop("The response must be a numeric variable.") 

n <- length(y)
x.levels <- levels(factor(group))
k <- length(x.levels)
y.means <- y.n <- y.vars <- NULL


for (i in x.levels) {
y.means[i] <- mean(y[group==i])
y.n[i] <- length(y[group==i])
y.vars[i] <- var(y[group==i])
}


w <- y.n/y.vars
h <- w/sum(w)


A <- sum((1 - w/sum(w))^2/(y.n - 1))
c <- (k - 1) + 2 * A - (6 * A/(k + 1)) 
T <- sum(w * (y.means - sum(h * y.means))^2)

df1 <- k-1
df2 <- (k - 1) * (k + 1)/(3 * A)

Ftest=T/c

p.value=pf(Ftest,df1,df2,lower.tail = F)

  


if (verbose) {
            cat("\n", "",METHOD,paste("(alpha = ",alpha,")",sep = ""), "\n", 
                sep = " ")
            cat("-------------------------------------------------------------", 
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
            cat("-------------------------------------------------------------", 
                "\n\n", sep = " ")
        }

result <- list()
result$statistic <- Ftest
result$parameter <- c(df1,df2)
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD 
result$data <- data
result$formula <- formula

attr(result, "class") <- "owt"
invisible(result)


}



