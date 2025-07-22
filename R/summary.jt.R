
summary.jt <- function(object, detail = TRUE, ...) {
  if (!inherits(object, "jt")) {
    stop("This object is not of class 'jt'.")
  }
  
  if(object$method == "James Second Order Test"){
    cat("\n", "",object$method,paste("(alpha = ",object$alpha,")",sep = ""), "\n", 
        sep = " ")
    header_line <- paste(object$method, paste("(alpha =", object$alpha, ")"))
    line_length <- max(nchar(header_line), 60)
    line_sep <- strrep("-", (line_length+38))
    cat(line_sep, "\n", sep = " ")
    dep_var <- deparse(object$formula[[2]])
    grp_var <- deparse(object$formula[[3]])
    cat("  dependent var :", dep_var, "\n")
    cat("  grouping var  :", grp_var, "\n\n")
    cat("  Jtest         =", object$statistic, "\n", sep = " ")
    cat("  CriticalValue =", object$criticalValue, "\n\n", sep = " ")
    cat(if (object$statistic < object$criticalValue) {
      "  Result : Difference is not statistically significant."
      cat(line_sep, "\n", sep = " ")
    }
    else {
      "  Result : Difference is statistically significant."
    }, "\n")
    cat(line_sep, "\n", sep = " ")
    if (detail == TRUE) {
      cat("  * test statistic is sum of the squared standardized differences and compared to a critical value. \n")}
  }
}

