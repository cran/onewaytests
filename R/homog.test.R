
homog.test <- function(y, group, method = c("Levene", "Bartlett", "Fligner"), alpha = 0.05, na.rm = TRUE, verbose = TRUE){

dname1 <- deparse(substitute(y))
dname2 <- deparse(substitute(group))
DNAME <- paste(dname1, "and", dname2)
method = match.arg(method)
if (na.rm) {
        completeObs <- complete.cases(y, group)
        y <- y[completeObs]
        group <- group[completeObs]
    }

if (method == "Levene"){
out=leveneTest(y, group, center="mean")
if (verbose) {
            cat("\n", " Levene's Homogeneity Test", "\n", 
                sep = " ")
            cat("-----------------------------------------------", 
                "\n", sep = " ")
            cat("  data :", DNAME, "\n\n", sep = " ")
            cat("  statistic  :", out$F[1], "\n", sep = " ")
            cat("  num df     :", out$Df[1], "\n", sep = " ")
		 cat("  denum df   :", out$Df[2], "\n", sep = " ")
            cat("  p.value    :", out$P[1], "\n\n", sep = " ")
            cat(if (out$P[1] > alpha) {
                "  Result     : Variances are homogeneous."
            }
            else {
                "  Result     : Variances are not homogeneous."
            }, "\n")
            cat("-----------------------------------------------", 
                "\n\n", sep = " ")
        }

result <- list()
result$statistic <- out$F[1]
result$parameter <- out$Df
result$p.value <- out$P[1]
result$method <- "Levene's Homogeneity Test"
result

}

if (method == "Bartlett"){
out=bartlett.test(y, group)
if (verbose) {
            cat("\n", " Bartlett's Homogeneity Test", "\n", 
                sep = " ")
            cat("-----------------------------------------------", 
                "\n", sep = " ")
            cat("  data :", DNAME, "\n\n", sep = " ")
            cat("  statistic  :", out$statistic, "\n", sep = " ")
            cat("  parameter  :", out$parameter, "\n", sep = " ")
		 cat("  p.value    :", out$p.value, "\n\n", sep = " ")
            cat(if (out$p.value > alpha) {
                "  Result     : Variances are homogeneous."
            }
            else {
                "  Result     : Variances are not homogeneous."
            }, "\n")
            cat("-----------------------------------------------", 
                "\n\n", sep = " ")
        }

result <- list()
result$statistic <- as.numeric(out$statistic)
result$parameter <- as.numeric(out$parameter)
result$p.value <- out$p.value
result$method <- "Bartlett's Homogeneity Test"

}


if (method == "Fligner"){
out=fligner.test(y, group)
if (verbose) {
            cat("\n", " Fligner-Killeen Homogeneity Test", "\n", 
                sep = " ")
            cat("-----------------------------------------------", 
                "\n", sep = " ")
            cat("  data :", DNAME, "\n\n", sep = " ")
            cat("  statistic  :", out$statistic, "\n", sep = " ")
            cat("  parameter  :", out$parameter, "\n", sep = " ")
		 cat("  p.value    :", out$p.value, "\n\n", sep = " ")
            cat(if (out$p.value > alpha) {
                "  Result     : Variances are homogeneous."
            }
            else {
                "  Result     : Variances are not homogeneous."
            }, "\n")
            cat("-----------------------------------------------", 
                "\n\n", sep = " ")
        }

result <- list()
result$statistic <- as.numeric(out$statistic)
result$parameter <- as.numeric(out$parameter)
result$p.value <- out$p.value
result$method <- "Fligner-Killeen Homogeneity Test"

}

invisible(result)
}