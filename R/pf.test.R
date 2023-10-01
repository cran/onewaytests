pf.test <- function(formula, data, N = 10^5, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  
  METHOD <- "Permutation F Test"
  
  
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
  
  
  F_observed <- aov.test(formula, data, alpha = alpha, na.rm = na.rm, verbose = FALSE)$statistic
  count <- 0
  for (i in 1:N) {
    group_p <- sample(group)
    store <- data.frame(y, group_p)
    F_p <- aov.test(y ~ group_p, data = store, alpha = alpha, na.rm = na.rm, verbose = FALSE)$statistic
    if (F_p >= F_observed) count <- count + 1
  }
  
  p.value <- count/N
  
  
  if (verbose) {
    cat("\n", "",METHOD,paste("(alpha = ",alpha,")",sep = ""), "\n", 
        sep = " ")
    cat("-------------------------------------------------------------", 
        "\n", sep = " ")
    cat("  data :", DNAME, "\n\n", sep = " ")
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
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD 
  result$data <- data
  result$formula <- formula
  result$N <- N
  
  attr(result, "class") <- "owt"
  invisible(result)
  
  
}



