ss.test <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  
  METHOD <- "Scott-Smith Test"
  
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
  
  y.means <- tapply(y, group, mean)
  
  y.n <- tapply(y, group, length)
  
  y.avar <- ((y.n-1)/(y.n-3))*tapply(y, group, var)
  
  
  T <- y.n*((y.means-mean(y))^2)/y.avar
  
  
  
  approx <- sum(T)
  df <- length(x.levels)
  p.value <- pchisq(approx, df=df, lower.tail=FALSE)
  
  
  
  if (verbose) {
    cat("\n", "",METHOD, paste("(alpha = ",alpha,")",sep = ""), "\n", 
        sep = " ")
    cat("-------------------------------------------------------------", 
        "\n", sep = " ")
    cat("  data :", DNAME, "\n\n", sep = " ")
    cat("  statistic  :", approx, "\n", sep = " ")
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
