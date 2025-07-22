bf.test <- function (formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE){
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  
  METHOD <- "Brown-Forsythe Test"
  
  
  if (na.rm){
    completeObs <- complete.cases(data)
    data <- data[completeObs,]
  }
  
  if (any(colnames(data)==dp[[3L]])==FALSE) stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
  if (any(colnames(data)==dp[[2L]])==FALSE) stop("The name of response variable does not match the variable names in the data.")
  
  y = data[[dp[[2L]]]]
  group = data[[dp[[3L]]]]
  
  
  if (!(is.factor(group)|is.character(group))) stop("The group variable must be a factor or a character.") 
  if (is.character(group)) group <- as.factor(group) 
  if (!is.numeric(y)) stop("The response must be a numeric variable.") 
  
  n <- length(y)
  x.levels <- levels(factor(group))
  y.mean = mean(y)
  y.means <- tapply(y, group, mean)
  y.n <- tapply(y, group, length)
  y.vars <- tapply(y, group, var)
  m <- (1 - y.n/n) * (y.vars)/sum((1 - y.n/n) * (y.vars))
  SSb = sum(y.n * ((y.means - y.mean)^2))
  denom = sum((1 - y.n/n) * (y.vars))
  Ftest = SSb/denom
  df1 = length(x.levels) - 1
  df2 = 1/(sum(m^2/(y.n - 1)))
  p.value = pf(Ftest, df1, df2, lower.tail = F)
  
  if (verbose) {
  print(structure(list(statistic = c("F" = Ftest), parameter = c("num df" = df1, "denom df" = df2), 
                 p.value = p.value, method = METHOD, data.name = DNAME), class = "htest"))
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
