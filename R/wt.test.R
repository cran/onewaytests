wt.test <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  
  METHOD <- "Welch's t-Test"
  
  
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
  if (length(levels(factor(group)))!=2) stop("The group variable must have two levels.")
  
  n <- length(y)
  
  x.levels <- levels(factor(group))
  
  
  y.means <- tapply(y, group, mean)
  
  y.n <- tapply(y, group, length)
  
  y.var <- tapply(y, group, var)
  
  Ttest <- (y.means[1]-y.means[2])/sqrt(y.var[1]/y.n[1]+y.var[2]/y.n[2])
  
  df <- ((y.var[1]/y.n[1]+y.var[2]/y.n[2])^2)/((y.var[1]/y.n[1])^2/(y.n[1]-1)+(y.var[2]/y.n[2])^2/(y.n[2]-1))
  
  p.value <- 2*pt(abs(Ttest),df,lower.tail = F)
  
  if (verbose) {
  print(structure(list(statistic = setNames(Ttest, "t"), parameter = setNames(df, "df"), 
                 p.value = p.value, method = METHOD, data.name = DNAME), class = "htest"))
  }
  
  result <- list()
  result$statistic <- Ttest
  result$parameter <- df
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD 
  result$data <- data
  result$formula <- formula
  
  attr(result, "class") <- "owt"
  invisible(result)
  
}

