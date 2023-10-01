wa.test <- function (formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Welch-Aspin Test"
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
  y.means <- tapply(y, group, mean)
  y.vars <- tapply(y, group, var)
  y.n <- tapply(y, group, length)
  y.w <- (1/(y.vars/y.n))/sum(1/(y.vars/y.n))
  wmean <- sum(y.w * y.means)
  t <- (y.means - wmean)/sqrt(y.vars/y.n)
  k <- length(x.levels)
  v <- y.n - 1
  lambda = sum((1 - y.w)^2/v)
  
  v1 <- k - 1
  v2 <- (k^2-1)/(3*lambda)
  
  Ftest <- (sum(t^2)/(k-1))/(1+(2*k-2)/(k^2-1)*lambda)
  
  p.value = pf(Ftest, v1, v2, lower.tail = F)
  if (verbose) {
    cat("\n", "", METHOD, paste("(alpha = ", 
                                alpha, ")", sep = ""), "\n", sep = " ")
    cat("-------------------------------------------------------------", 
        "\n", sep = " ")
    cat("  data :", DNAME, "\n\n", sep = " ")
    cat("  statistic  :", Ftest, "\n", sep = " ")
    cat("  num df     :", v1, "\n", sep = " ")
    cat("  denom df   :", v2, "\n", sep = " ")
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
  result$parameter <- c(v1, v2)
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  attr(result, "class") <- "owt"
  invisible(result)
}
