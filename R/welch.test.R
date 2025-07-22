
welch.test <- function(formula, data, rate = 0, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  
  if (rate==0){METHOD <- "Welch's Heteroscedastic F Test"
  }else{METHOD <- "Welch's Heteroscedastic F Test with Trimmed Means and Winsorized Variances"}
  
  if (na.rm){
    completeObs <- complete.cases(data)
    data <- data[completeObs,]
  }
  
  if (any(colnames(data)==dp[[3L]])==FALSE) stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
  if (any(colnames(data)==dp[[2L]])==FALSE) stop("The name of response variable does not match the variable names in the data.")
  
  y=data[[dp[[2L]]]]
  group=as.factor(data[[dp[[3L]]]])
  
  
  if (!(is.factor(group)|is.character(group))) stop("The group variable must be a factor or a character.") 
  if (is.character(group)) group <- as.factor(group)
  if (!is.numeric(y)) stop("The response must be a numeric variable.") 
  
  trim=function(x){
    n=length(x)
    xx=sort(x)
    lambda=round(n*rate)
    xx[(lambda+1):(n-lambda)]
  }
  
  wins=function(x){
    n=length(x)
    xx=sort(x)
    lambda=round(n*rate)
    xxx=c(rep(xx[lambda+1],lambda),xx[(lambda+1):(n-lambda)],rep(xx[n-lambda],lambda))
    xxx
  }
  
  n <- length(y)
  x.levels <- levels(factor(group))
  
  y.n <- tapply(y, group, length)
  
  lambda=round(y.n*rate)
  
  b=y.n-2*lambda
  
  y.vars <- sapply(tapply(y, group, wins), var)
  
  y.means <- sapply(tapply(y, group, trim), mean)
  
  q=(y.n-1)*y.vars/b/(b-1)
  
  w <- 1/q
  
  U=sum(w)
  
  w_y=sum(w*y.means)/U
  
  J=length(x.levels)
  
  A=sum(w*(y.means-w_y)^2)/(J-1)
  
  B=2*(J-2)/(J^2-1)*sum((1-w/U)^2/(b-1))
  
  
  Ftest=A/(B+1)
  
  df1=J-1
  df2=(3/(J^2-1)*sum((1-w/U)^2/(b-1)))^(-1)
  
  
  p.value=pf(Ftest,df1,df2,lower.tail = F)
  
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
  result$rate <- rate
  result$data <- data
  result$formula <- formula
  
  
  attr(result, "class") <- "owt"
  invisible(result)
  
}

