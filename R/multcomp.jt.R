
multcomp.jt<- function(x, adjust.method = c("bonferroni", "none"), alpha = 0.05,...){

y <- x$y
group <- x$group
id <- levels(group)
comb <- t(combn((id), 2))
comb2 <- dim(comb)[1]

adjust.method <- match.arg(adjust.method)
if (adjust.method == "bonferroni"){ 
alpha_adj=alpha/comb2
method.name = paste("Bonferroni Correction (alpha = ",alpha_adj,")",sep = "")
}
if (adjust.method == "none"){ 
alpha_adj=alpha
method.name = paste("No Correction (alpha = ",alpha_adj,")",sep = "")
}


statistics <- NULL
cvs <- NULL
for (i in 1:comb2){
    y_sub <- y[(group == comb[i,1])|(group == comb[i,2])]
    group_sub <- group[(group == comb[i,1])|(group == comb[i,2])]
if (x$method == "James Second Order Test") {
statistics <- c(statistics , james.test(y_sub,group_sub,alpha=alpha_adj,verbose=F)$statistic)
cvs <- c(cvs , james.test(y_sub,group_sub,alpha=alpha_adj,verbose=F)$criticalValue)

}
}


store = data.frame(matrix(NA, nrow = comb2, ncol = 5))
    
    store$X1 = comb[,1]
    store$X2 = comb[,2]
    store$X3 = statistics 
    store$X4 = cvs 
    store$X5 = ifelse(store$X3 < store$X4 , "NO", "YES")
    colnames(store) = c("Level (a)", "Level (b)", "statistic","criticalValue", "Difference")

cat("\n", "",method.name, "\n", sep = " ")
cat("-----------------------------------------------------------", "\n", sep = " ")
print(store)
cat("-----------------------------------------------------------", "\n\n", sep = " ")


invisible(store)

}

