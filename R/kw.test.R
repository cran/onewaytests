kw.test <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE){
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  
  METHOD <- "Kruskal-Wallis Test"
  
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
  
  ranks = rank(y)
  N<-length(y)
  x.levels <- levels(factor(group))
  t<-as.data.frame(table(rank(y)))
  ties<-t$Freq
  n.i <- tapply(y, group, length)
  Rmean.i <- tapply(ranks, group, mean)
  
  
  KW.noties = 12/(N*(N+1)) * sum( n.i*(Rmean.i - (N+1)/2)^2 )
  
  correction <- (1 - sum( ties^3 - ties )/(N^3 - N) )
  approx<- KW.noties/correction
  p.value<-pchisq(approx, df=(length(x.levels)-1),lower.tail = FALSE)
  df=(length(x.levels)-1)
  
  if (verbose) {
  print(structure(list(statistic = c("X-squared" = approx), parameter = c("df" = df), 
                 p.value = p.value, method = METHOD, data.name = DNAME), class = "htest"))
  }
  
  result <- list()
  result$statistic <- approx
  result$parameter <- df
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD 
  result$data <- data
  result$formula <- formula
  
  attr(result, "class") <- "owt"
  invisible(result)
  
}
