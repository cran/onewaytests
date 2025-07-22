
summary.owt <- function(object, detail = TRUE, ...) {
  if (!inherits(object, "owt")) {
    stop("This object is not of class 'owt'.")
  }
  
  if(object$method == "Alvandi's F Test" | object$method =="One-Way Analysis of Variance" 
     | object$method == "Approximate F Test" | object$method == "Adjusted Welch's Heteroscedastic F Test" 
     | object$method == "Brown-Forsythe Test" | object$method == "Box F Test" 
     | object$method == "Johansen F Test" | object$method == "Modified Brown-Forsythe Test"
     | object$method == "Welch-Aspin Test" | object$method == "Welch's Heteroscedastic F Test"
     | object$method == "Welch's Heteroscedastic F Test with Trimmed Means and Winsorized Variances"){
    cat("\n", "",object$method,paste("(alpha = ",object$alpha,")",sep = ""), "\n", 
        sep = " ")
    header_line <- paste(object$method, paste("(alpha =", object$alpha, ")"))
    line_length <- max(nchar(header_line), 60)
    line_sep <- strrep("-", (line_length+1))
    cat(line_sep, "\n", sep = " ")
    dep_var <- deparse(object$formula[[2]])
    grp_var <- deparse(object$formula[[3]])
    cat("  dependent var :", dep_var, "\n")
    cat("  grouping var  :", grp_var, "\n\n")
    cat("  F        =", object$statistic, "\n", sep = " ")
    cat("  num df   =", object$parameter[[1]], "\n", sep = " ")
    cat("  denom df =", object$parameter[[2]], "\n", sep = " ")
    if(object$p.value < 2.2e-16) cat("  p-value  < 2.2e-16\n\n") else cat("  p-value  =", format.pval(object$p.value, ...), "\n\n", sep = " ")
    cat(if (object$p.value > object$alpha) {
      "  Result : Difference is not statistically significant."
    }
    else {
      "  Result : Difference is statistically significant."
    }, "\n")
    cat(line_sep, "\n", sep = " ")
    if (detail == TRUE) {
      cat("  * test statistic is distributed as F distribution. \n")}
  }
  
  else if(object$method == "Alexander-Govern Test" | object$method == "B Square Test"
           | object$method == "Cochran Test" | object$method == "Kruskal-Wallis Test"
           | object$method == "Scott-Smith Test"){
    cat("\n", "",object$method,paste("(alpha = ",object$alpha,")",sep = ""), "\n", 
        sep = " ")
    header_line <- paste(object$method, paste("(alpha =", object$alpha, ")"))
    line_length <- max(nchar(header_line), 60)
    line_sep <- strrep("-", (line_length+3))
    cat(line_sep, "\n", sep = " ")
    dep_var <- deparse(object$formula[[2]])
    grp_var <- deparse(object$formula[[3]])
    cat("  dependent var :", dep_var, "\n")
    cat("  grouping var  :", grp_var, "\n\n")
    cat("  X-squared =", object$statistic, "\n", sep = " ")
    cat("  df        =", object$parameter, "\n", sep = " ")
    if(object$p.value < 2.2e-16) cat("  p-value   < 2.2e-16\n\n") else cat("  p-value  =", format.pval(object$p.value, ...), "\n\n", sep = " ")
    cat(if (object$p.value > object$alpha) {
      "  Result : Difference is not statistically significant."
    }
    else {
      "  Result : Difference is statistically significant."
    }, "\n")
    cat(line_sep, "\n", sep = " ")
    if (detail == TRUE) {
      cat("  * test statistic is distributed as chi-squared distribution. \n")}
  }
  
  else if(object$method == "Alvandi's Generalized P-Value" | object$method == "Weerahandi's Generalized F Test"){
    cat("\n", "",object$method,paste("(alpha = ",object$alpha,")",sep = ""), "\n", 
        sep = " ")
    header_line <- paste(object$method, paste("(alpha =", object$alpha, ")"))
    line_length <- max(nchar(header_line), 60)
    line_sep <- strrep("-", (line_length+1))
    cat(line_sep, "\n", sep = " ")
    dep_var <- deparse(object$formula[[2]])
    grp_var <- deparse(object$formula[[3]])
    cat("  dependent var :", dep_var, "\n")
    cat("  grouping var  :", grp_var, "\n\n")
    if(object$p.value < 2.2e-16) cat("  p-value < 2.2e-16\n\n") else cat("  p-value =", format.pval(object$p.value, ...), "\n\n", sep = " ")
    cat(if (object$p.value > object$alpha) {
      "  Result : Difference is not statistically significant."
    }
    else {
      "  Result : Difference is statistically significant."
    }, "\n")
    cat(line_sep, "\n", sep = " ")
    if (detail == TRUE) {
      cat("  * p-value is obtained using Monte Carlo simulation. \n")}
  }
  
  else if(object$method == "Generalized Test Equivalent to Fiducial Test (size assured)" 
          | object$method == "Generalized Test Equivalent to Parametric Bootstrap Test (size close to intended)"){
    cat("\n", "",object$method,paste("(alpha = ",object$alpha,")",sep = ""), "\n", 
        sep = " ")
    header_line <- paste(object$method, paste("(alpha =", object$alpha, ")"))
    line_length <- max(nchar(header_line), 60)
    line_sep <- strrep("-", (line_length+1))
    cat(line_sep, "\n", sep = " ")
    dep_var <- deparse(object$formula[[2]])
    grp_var <- deparse(object$formula[[3]])
    cat("  dependent var :", dep_var, "\n")
    cat("  grouping var  :", grp_var, "\n\n")
    if(object$p.value < 2.2e-16) cat("  p-value  < 2.2e-16\n\n") else cat("  p-value  =", format.pval(object$p.value, ...), "\n\n", sep = " ")
    cat(if (object$p.value > object$alpha) {
      "  Result : Difference is not statistically significant."
    }
    else {
      "  Result : Difference is statistically significant."
    }, "\n")
    cat(line_sep, "\n", sep = " ")
    if (detail == TRUE) {
      if (object$method == "Generalized Test Equivalent to Fiducial Test (size assured)") {
        cat("  * p-value is obtained using generalized fiducial method. \n")}        
      
      else if (object$method == "Generalized Test Equivalent to Parametric Bootstrap Test (size close to intended)") {
        cat("  * p-value is obtained using parametric bootstrap method. \n")} 
    }
  }
  
  else if(object$method == "Permutation F Test"){
    cat("\n", "",object$method,paste("(alpha = ",object$alpha,")",sep = ""), "\n", 
        sep = " ")
    header_line <- paste(object$method, paste("(alpha =", object$alpha, ")"))
    line_length <- max(nchar(header_line), 60)
    line_sep <- strrep("-", (line_length+15))
    cat(line_sep, "\n", sep = " ")
    dep_var <- deparse(object$formula[[2]])
    grp_var <- deparse(object$formula[[3]])
    cat("  dependent var :", dep_var, "\n")
    cat("  grouping var  :", grp_var, "\n\n")
    if(object$p.value < 2.2e-16) cat("  p-value  < 2.2e-16\n\n") else cat("  p-value  =", format.pval(object$p.value, ...), "\n\n", sep = " ")
    cat(if (object$p.value > object$alpha) {
      "  Result : Difference is not statistically significant."
    }
    else {
      "  Result : Difference is statistically significant."
    }, "\n")
    cat(line_sep, "\n", sep = " ")
    if (detail == TRUE) {
      cat("  * p-value is based on empirical distribution generated via permutation. \n")}
  }
  
  else if(object$method == "Mann-Whitney U Test"){
    cat("\n", "",object$method,paste("(alpha = ",object$alpha,")",sep = ""), "\n", 
        sep = " ")
    header_line <- paste(object$method, paste("(alpha =", object$alpha, ")"))
    line_length <- max(nchar(header_line), 60)
    line_sep <- strrep("-", (line_length+7))
    cat(line_sep, "\n", sep = " ")
    group = object$data[[deparse(object$formula[[3]])]]
    x.levels <- levels(factor(group))
    DNAME <- paste(x.levels[1], "vs.", x.levels[2])
    cat("  Groups :", DNAME, "\n\n", sep = " ")
    cat("  z       =", object$statistic, "\n", sep = " ")
    if(object$p.value < 2.2e-16) cat("  p-value < 2.2e-16\n\n") else cat("  p-value =", format.pval(object$p.value), "\n\n", sep = " ")
    cat(if (object$p.value > object$alpha) {
      "  Result : Difference is not statistically significant."
    }
    else {
      "  Result : Difference is statistically significant."
    }, "\n")
    cat(line_sep, "\n", sep = " ")
    if (detail == TRUE) {
      cat("  * test statistic is distributed as standard normal distribution. \n")}
  }
  
  else if(object$method == "Student's t-Test" | object$method == "Welch's t-Test"){
    cat("\n", "",object$method,paste("(alpha = ",object$alpha,")",sep = ""), "\n", 
        sep = " ")
    header_line <- paste(object$method, paste("(alpha =", object$alpha, ")"))
    line_length <- max(nchar(header_line), 60)
    line_sep <- strrep("-", (line_length+1))
    cat(line_sep, "\n", sep = " ")
    group = object$data[[deparse(object$formula[[3]])]]
    x.levels <- levels(factor(group))
    DNAME <- paste(x.levels[1], "vs.", x.levels[2])
    cat("  Groups :", DNAME, "\n\n", sep = " ")
    cat("  t       =", object$statistic, "\n", sep = " ")
    cat("  df      =", object$parameter, "\n", sep = " ")
    if(object$p.value < 2.2e-16) cat("  p-value < 2.2e-16\n\n") else cat("  p-value =", format.pval(object$p.value), "\n\n", sep = " ")
    cat(if (object$p.value > object$alpha) {
      "  Result : Difference is not statistically significant."
    }
    else {
      "  Result : Difference is statistically significant."
    }, "\n")
    cat(line_sep, "\n", sep = " ")
    if (detail == TRUE) {
      cat("  * test statistic is distributed as t-distribution. \n")}
  }
}

