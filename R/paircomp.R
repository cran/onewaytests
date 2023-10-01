paircomp <- function(x, ...) UseMethod("paircomp")

paircomp.default <- function(x, ...) paircomp.owt(x, ...)


paircomp.owt<- function(x, adjust.method = c("bonferroni", "holm", "hochberg", "hommel", "BH", 
                                             "BY", "fdr", "none"), verbose = TRUE, ...){
  
  if(x$p.value>x$alpha) stop(paste("Pairwise comparisons could not be performed since difference is not statistically significant (alpha = ",x$alpha,").",sep = ""))
  
  data<-x$data
  
  dp=as.character(x$formula)
  
  y=data[[dp[[2L]]]]
  group=as.factor(data[[dp[[3L]]]])
  
  if (is.character(group)) group <- as.factor(group)
  
  if (length(levels(group))==2) stop("The group variable must have more than two levels to make pairwise comparison.")
  
  
  alpha <- x$alpha
  id <- levels(group)
  comb <- t(combn((id), 2))
  comb2 <- dim(comb)[1]
  
  pval <- NULL
  for (i in 1:comb2){
    data_sub <- data[(group == comb[i,1])|(group == comb[i,2]),]
    data_sub[[dp[[3L]]]] <- factor(data_sub[[dp[[3L]]]])
    if (x$method == "One-Way Analysis of Variance") pval <- c(pval, aov.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "Alvandi's F Test") pval <- c(pval, af.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "Approximate F Test") pval <- c(pval, ap.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "B Square Test") pval <- c(pval, b2.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "Alvandi's Generalized P-Value") pval <- c(pval, agp.test(x$formula,data_sub,verbose=F,N=x$N)$p.value)
    if (x$method == "Alexander-Govern Test") pval <- c(pval, ag.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "Cochran Test") pval <- c(pval, cochran.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "Weerahandi's Generalized F Test") pval <- c(pval, wgf.test(x$formula,data_sub,verbose=F,N=x$N)$p.value)
    if (x$method == "Permutation F Test") pval <- c(pval, pf.test(x$formula,data_sub,verbose=F,N=x$N)$p.value)
    if (x$method == "Brown-Forsythe Test") pval <- c(pval, bf.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "Kruskal-Wallis Test") pval <- c(pval, kw.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "Welch-Aspin Test") pval <- c(pval, wa.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "Scott-Smith Test") pval <- c(pval, ss.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "Modified Brown-Forsythe Test") pval <- c(pval, mbf.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "Johansen F Test") pval <- c(pval, johansen.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "Adjusted Welch's Heteroscedastic F Test") pval <- c(pval, aw.test(x$formula,data_sub,verbose=F)$p.value)
    if (x$method == "Generalized Test Equivalent to Fiducial Test (size assured)") pval <- c(pval, gp.test(x$formula,data_sub, method = "gtf",verbose=F)$p.value)
    if (x$method == "Generalized Test Equivalent to Parametric Bootstrap Test (size close to intended)") pval <- c(pval, gp.test(x$formula,data_sub, method = "gtb",verbose=F)$p.value)
    if (x$method == "Box F Test") pval <- c(pval, box.test(x$formula,data_sub,verbose=F)$p.value)
    if ((x$method == "Welch's Heteroscedastic F Test")|(x$method == "Welch's Heteroscedastic F Test with Trimmed Means and Winsorized Variances")) pval <- c(pval, welch.test(x$formula,data_sub, rate=x$rate,verbose=F)$p.value)
    
    
    
    
    
    
  }
  
  adjust.method <- match.arg(adjust.method)
  
  if (adjust.method == "bonferroni") method.name = paste("Bonferroni Correction (alpha = ",alpha,")",sep = "")
  if (adjust.method == "holm") method.name = paste("Holm Correction (alpha = ",alpha,")",sep = "")
  if (adjust.method == "hochberg") method.name = paste("Hochberg Correction (alpha = ",alpha,")",sep = "")
  if (adjust.method == "hommel") method.name = paste("Hommel Correction (alpha = ",alpha,")",sep = "")
  if ((adjust.method == "fdr")|(adjust.method == "BH")) method.name = paste("Benjamini-Hochberg Correction (alpha = ",alpha,")",sep = "")
  if (adjust.method == "BY") method.name = paste("Benjamini-Yekutieli Correction (alpha = ",alpha,")",sep = "")
  if (adjust.method == "none") method.name = paste("No Correction (alpha = ",alpha,")",sep = "")
  
  
  padj <- p.adjust(pval, method = adjust.method)
  
  
  
  store = data.frame(matrix(NA, nrow = comb2, ncol = 4))
  
  store$X1 = comb[,1]
  store$X2 = comb[,2]
  store$X3 = padj
  store$X4 = ifelse(store$X3 <= alpha, "Reject", "Not reject")
  colnames(store) = c("Level (a)", "Level (b)", "p.value", "  No difference")
  
  if (verbose) {
    cat("\n", "",method.name, "\n", sep = " ")
    cat("-----------------------------------------------------", "\n", sep = " ")
    print(store)
    cat("-----------------------------------------------------", "\n\n", sep = " ")
  }
  
  invisible(store)
  
}

