gp.test<-function(formula, data, method = c("gtb","gtf"), alpha = 0.05, na.rm = TRUE, verbose = TRUE) {
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  
  
  nM <-1000000
  method = match.arg(method)
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
  
  
  g<-group
  k = nlevels(g)
  k1 = k - 1 ; k21 = k^2-1
  ns = tapply(y, g, length)
  rs = ns-1
  xb = tapply(y, g, mean) # Sample means
  sq = tapply(y, g, var) # Sample variances
  wts = ns/sq
  w = sum(wts)
  xbs = sum(ns*xb/sq)/sum(ns/sq) # TO CHECK
  rhs = sum(wts*(xb-xbs)^2)
  
  # Compue Genralized Test using random T variables: Generalized Test Equivalent to Fiducial Test
  Deno = w # sum(ns/sq)
  sqw = sqrt(wts)
  # Compue Genralized Test using random Chi and Z variables: Generalized Test Equivalent to PB Test
  sqn = sqrt(ns)
  sd = sqrt(sq)
  sqr = sqrt(rs)
  
  T1 = T2 = P1 = P2 = P3 = 0
  for (i in 1:k) {
    Zvec = rnorm(nM)
    Zvec = Zvec - mean(Zvec) # Bias corrected random numbers
    Chivec = rchisq(nM, rs[i])
    Chivec = rs[i] * Chivec / mean(Chivec)	
    Tvec = sqr[i]*Zvec /sqrt(Chivec)
    Tvec2 = Tvec^2 # Square of T
    # Comput Gen. Test based on Tvec
    T1 = T1 + Tvec2
    T2 = T2 + Tvec*sqw[i]	
    P2 = P2 + sqw[i]*Zvec*rs[i] / Chivec
    P3 = P3 + wts[i]*rs[i]/ Chivec
    
  }
  
  Del = T1 - T2^2/Deno
  g1pval = mean(Del > rhs)
  
  P1 = T1
  pb = P1 - P2^2 / P3
  
  g2pval = mean(pb > rhs)
  
  if(method == "gtf"){
    METHOD <- "Generalized Test Equivalent to Fiducial Test (size assured)"
    p.value <- g1pval
  }
  
  if(method == "gtb"){
    METHOD <- "Generalized Test Equivalent to Parametric Bootstrap Test (size close to intended)"
    p.value <- g2pval
  }
  
  
  if (verbose) {
    cat("\n", "",METHOD, paste("(alpha = ",alpha,")",sep = ""), "\n", 
        sep = " ")
    line<-paste(rep("-",nchar(METHOD)+19),sep = "")
    cat(line, 
        "\n", sep = "")
    cat("  data :", DNAME, "\n\n", sep = " ")
    
    
    cat("  p.value    :", p.value, "\n\n", sep = " ")
    cat(if (p.value > alpha) {
      "  Result     : Difference is not statistically significant."
    }
    else {
      "  Result     : Difference is statistically significant."
    }, "\n")
    cat(line, 
        "\n\n", sep = "")
  }
  
  
  
  
  result <- list()
  result$p.value <- p.value
  result$alpha <- alpha
  result$method <- METHOD
  result$data <- data
  result$formula <- formula
  
  
  attr(result, "class") <- "owt"
  invisible(result)	
  
  
}
