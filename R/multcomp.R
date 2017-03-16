multcomp <- function(x,...) UseMethod("multcomp")

multcomp.default <- function(x,...) multcomp.owt(x,...)


multcomp.owt<- function(x, adjust.method = c("bonferroni", "holm", "hochberg", "hommel", "BH", 
  "BY", "fdr", "none"), alpha = 0.05,...){

y <- x$y
group <- x$group
id <- levels(group)
comb <- t(combn((id), 2))
comb2 <- dim(comb)[1]

pval <- NULL
for (i in 1:comb2){
    y_sub <- y[(group == comb[i,1])|(group == comb[i,2])]
    group_sub <- group[(group == comb[i,1])|(group == comb[i,2])]
if (x$method == "One-Way Analysis of Variance") pval <- c(pval, aov.test(y_sub,group_sub,verbose=F)$p.value)
if (x$method == "Alexander-Govern Test") pval <- c(pval, ag.test(y_sub,group_sub,verbose=F)$p.value)
if (x$method == "Brown-Forsythe Test") pval <- c(pval, bf.test(y_sub,group_sub,verbose=F)$p.value)
if (x$method == "Kruskal-Wallis Test") pval <- c(pval, kw.test(y_sub,group_sub,verbose=F)$p.value)
if ((x$method == "Welch's Heteroscedastic F Test")|(x$method == "Welch's Heteroscedastic F Test with Trimmed Means and Winsorized Variances")) pval <- c(pval, welch.test(y_sub,group_sub, rate=x$rate,verbose=F)$p.value)


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
    store$X4 = ifelse(store$X3 <= alpha, "YES", "NO")
    colnames(store) = c("Level (a)", "Level (b)", "p.value", "Difference")

cat("\n", "",method.name, "\n", sep = " ")
cat("-------------------------------------------------", "\n", sep = " ")
print(store)
cat("-------------------------------------------------", "\n\n", sep = " ")


invisible(store)

}

