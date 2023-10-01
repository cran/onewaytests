agp.test <- function (formula, data, N = 10^5, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)  
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Alvandi's Generalized P-Value"
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
  y.mean <- mean(y)
  y.means <- tapply(y, group, mean)
  y.n <- tapply(y, group, length)
  y.vars <- tapply(y, group, var)
  y.se <- sqrt(y.vars/y.n)
  qj <- sqrt((y.n/y.vars)/(sum(y.n/y.vars)))
  X_bar_w <- sum(qj*y.means)
  Tw <- sum((y.n/y.vars)*((y.means-X_bar_w)^2))
  p <- 0
  for(i in 1:N){
    xj <- rnorm(k)
    uj <- rchisq(k, y.n - 1)
    x_tilda <- sum(qj*xj)
    T <- sum(((y.n-1)/uj)*((xj-qj*x_tilda)^2))
    if (T >= Tw) {
      p <- p + 1
    }
  }
  p.value <- p/N
  if (verbose) {
    cat("\n", "", METHOD, paste("(alpha = ", 
                                alpha, ")", sep = ""), "\n", sep = " ")
    cat("-------------------------------------------------------------", 
        "\n", sep = " ")
    cat("  p.value    :", round(p.value,4), "\n\n", sep = " ")
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
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  result$N <- N
  attr(result, "class") <- "owt"
  invisible(result)
}
