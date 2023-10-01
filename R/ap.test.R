ap.test <- function (formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Approximate F Test"
  if (na.rm) {
    completeObs <- complete.cases(data)
    data <- data[completeObs, ]
  }
  if (any(colnames(data) == dp[[3L]]) == FALSE) 
    stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
  if (any(colnames(data) == dp[[2L]]) == FALSE) 
    stop("The name of response variable does not match the variable names in the data.")
  y = data[[dp[[2L]]]]
  group = data[[dp[[3L]]]]
  if (!(is.factor(group) | is.character(group))) 
    stop("The group variable must be a factor or a character.")
  if (is.character(group)) 
    group <- as.factor(group)
  if (!is.numeric(y)) 
    stop("The response must be a numeric variable.")
  n <- length(y)
  x.levels <- levels(factor(group))
  k <- length(x.levels)
  grand.mean <- mean(y)
  y.means <- tapply(y, group, mean)
  y.n <- tapply(y, group, length)
  y.vars <- tapply(y, group, var)
  y.se <- sqrt(y.vars/y.n)
  U0 <- ((n-k)/(k-1))*((sum(y.n*(y.means-grand.mean)^2))/(sum((y.n-1)*(y.vars))))
  b <- ((n-k)/(n*(k-1)))*((sum((n-y.n)*(y.se)^2)))/sum((y.n-1)*(y.se)^2)
  df1 <- (sum((1-y.n/n)*y.vars))^2/(1/n^2*(sum(y.n*y.vars)^2)+sum((1-2*y.n/n)*y.vars^2))
  df2 <- (sum((y.n-1)*y.vars))^2/sum((y.n-1)*y.vars^2)
  Ftest <- U0/b
  p.value = pf(Ftest, df1, df2, lower.tail = F)
  if (verbose) {
    cat("\n", "", METHOD, paste("(alpha = ", 
                                alpha, ")", sep = ""), "\n", sep = " ")
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
  result$parameter <- c(df1, df2)
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  attr(result, "class") <- "owt"
  invisible(result)
}
