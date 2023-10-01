
wgf.test <- function(formula, data, N = 10^5, alpha = 0.05, na.rm = TRUE, verbose = TRUE) 
{
  data <- model.frame(formula, data)
  dp = as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Weerahandi's Generalized F Test"
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
  vq <- (y.n-1) * y.var 
  wts <- y.n/y.var
  ################## Compute p-value of Weerahandi Generalized F-test #########
  nsvq <- y.n/vq 
  gvec <- rep(0,N)
  Ymat <- matrix(0,k,N)
  stilde <- function(xbvec, wtvec)
  {
    prod <- wtvec * xbvec
    xbb <- sum(prod)/sum(wtvec)
    ss <- sum(wtvec*(xbvec^2)) - sum(prod)^2 /sum(wtvec)
    return(ss)
  }
  for (j in 1: k) {
    Ymat[j,] <- rchisq(N,(y.n-1)[j])
  }
  for (m in 1:N) { 
    Yv <- as.vector(Ymat[,m])
    wts <- nsvq*Yv		
    st <- stilde(y.means, wts) # Eqn (3.41) of Weerahandi paper
    gvec[m] <- pchisq(st,k-1)
  }
  gpval <-  1 - mean(gvec) # Eqn (3.41) of Weerahandi paper
  p.value <- round(c(gpval),3)
  if (verbose) {
    cat("\n", "", METHOD, paste("(alpha = ", 
                                alpha, ")", sep = ""), "\n", sep = " ")
    cat("-------------------------------------------------------------", 
        "\n", sep = " ")
    cat("  p.value    :", round(p.value,4), "\n\n", sep = " ")
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

