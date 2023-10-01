st.test <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  
  METHOD <- "Student's t-Test"
  
  
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
  
  
  y.means <- tapply(y, group, mean)
  
  y.var <- tapply(y, group, var)
  
  y.n <- tapply(y, group, length)
  
  Ttest <- (y.means[1]-y.means[2])/(sqrt(((y.n[1]-1)*y.var[1]+(y.n[2]-1)*y.var[2])/(y.n[1]+y.n[2]-2))*sqrt(1/y.n[1]+1/y.n[2]))
  
  df <- n-2
  
  p.value <- 2*pt(abs(Ttest),df,lower.tail = F)
  
  
  
  
  if (verbose) {
    cat("\n", "",METHOD, paste("(alpha = ",alpha,")",sep = ""), "\n", 
        sep = " ")
    cat("-------------------------------------------------------------", 
        "\n", sep = " ")
    cat("  Groups :", DNAME, "\n\n", sep = " ")
    cat("  statistic  :", Ttest, "\n", sep = " ")
    cat("  parameter  :", df, "\n", sep = " ")
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

