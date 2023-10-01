mw.test <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  
  METHOD <- "Mann-Whitney U Test"
  
  
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
  
  DNAME <- paste(x.levels[1], "vs.", x.levels[2])
  
  y.n <- tapply(y, group, length)
  
  
  data2 <- cbind(group,y)
  data2 <- data2[order(y),]
  data2 <- cbind(data2,1:length(y))
  means <- data.frame(tapply(data2[,3], data2[,2], mean))
  
  dummy <- NULL
  for (i in data2[,2]){
    ord<-which(rownames(means)==i)
    dummy<-c(dummy,means[ord,1])
  }
  data3 <- cbind(data2,dummy)
  #U1<-tapply(data3[,4], data3[,1], sum)[1]-y.n[1]*(y.n[1]+1)/2
  #U1 <- y.n[1]*y.n[2]+y.n[1]*(y.n[1]+1)/2-tapply(data3[,4], data3[,1], sum)[2]
  #U2 <- y.n[1]*y.n[2]-U1
  R1 <- tapply(data3[,4], data3[,1], sum)[1]
  R2 <- tapply(data3[,4], data3[,1], sum)[2]
  y.n1 <- tapply(data3[,4], data3[,1], length)[1]
  y.n2 <- tapply(data3[,4], data3[,1], length)[2]
  U1 <- R1 - y.n1*(y.n1+1)/2
  U2 <- R2 - y.n2*(y.n2+1)/2
  U <- min(U1,U2)
  
  if(length(y) == length(unique(y))){
    sigma_U <- sqrt(y.n1*y.n2*(y.n1+y.n2+1)/12)
  }else{
    nties <- table(y)
    sigma_U <- sqrt(y.n1*y.n2/(n*(n-1))*((n^3-n)/12-sum(nties^3-nties)/12))
    
  }
  
  Ztest <- abs(U - y.n1*y.n2/2)/sigma_U
  
  p.value <- 2*pnorm(abs(Ztest),lower.tail = F)
  
  
  
  
  if (verbose) {
    cat("\n", "",METHOD, paste("(alpha = ",alpha,")",sep = ""), "\n", 
        sep = " ")
    cat("-------------------------------------------------------------", 
        "\n", sep = " ")
    cat("  Groups :", DNAME, "\n\n", sep = " ")
    cat("  statistic  :", Ztest, "\n", sep = " ")
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
  result$statistic <- Ztest
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD 
  result$data <- data
  result$formula <- formula
  
  attr(result, "class") <- "owt"
  invisible(result)
  
  
}
