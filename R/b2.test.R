b2.test <- function (formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)  
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "B Square Test"
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
  y.var <- tapply(y, group, var)
  y.se <- sqrt(y.var/y.n)
  zc <- qnorm(1 - alpha/2)
  vj <- y.n-1
  c <- ((4*vj^2+(5*(2*zc^2+3)/24))/(4*vj^2+vj+(4*zc^2+9)/12))*sqrt(vj)
  wj <- (1/y.var)/sum(1/y.var)
  X.plus <- sum(wj*y.means)
  z <- c * sqrt(log(1 + (((y.means-X.plus)/y.se)^2/vj)))
  B2 <- sum(z^2) 
  df <- (k-1)
  p.value <- pchisq(B2, df = df, lower.tail = FALSE)
  if (verbose) {
  print(structure(list(statistic = c("X-squared" = B2), parameter = c("df" = df), 
                 p.value = p.value, method = METHOD, data.name = DNAME), class = "htest"))
  }
  result <- list()
  result$statistic <- B2
  result$parameter <- df
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  attr(result, "class") <- "owt"
  invisible(result)
}
