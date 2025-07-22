aw.test <- function (formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Adjusted Welch's Heteroscedastic F Test"
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
  y.c <- (y.n - 1)/(y.n - 3)
  y.w <- y.n/(y.c*y.vars)
  y.h <- y.w/sum(y.w)
  
  K <- length(x.levels)
  v <- (K^2-1)/(3*sum((1/(y.n-1))*((1-y.h)^2)))
  
  Ftest <- sum(y.w*((y.means-sum(y.h*y.means))^2))/((K-1)+2*(K-2)*(1/(K+1))*sum((1/(y.n-1))*((1-y.h)^2)))
  
  p.value = pf(Ftest, K-1, v, lower.tail = F)

  if (verbose) {
  print(structure(list(statistic = c("F" = Ftest), parameter = c("num df" = K-1, "denom df" = v), 
                 p.value = p.value, method = METHOD, data.name = DNAME), class = "htest"))
  }

  result <- list()
  result$statistic <- Ftest
  result$parameter <- c(K-1, v)
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  attr(result, "class") <- "owt"
  invisible(result)
}
