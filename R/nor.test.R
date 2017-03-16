
nor.test <- function(y, group, method = c("SW", "SF", "KS", "AD", "CVM", "PT"), alpha = 0.05, plot = c("qqplot-histogram", "qqplot", "histogram"), mfrow = NULL, na.rm = TRUE, verbose = TRUE){ 

    dname1 <- deparse(substitute(y))
    dname2 <- deparse(substitute(group))
    group = as.factor(group)
    k = length(levels(group))
    if (length(y) != length(group)) {
        stop("The lengths of y and group must be equal")
    }

method = match.arg(method)

if (na.rm) {
        completeObs <- complete.cases(y, group)
        y <- y[completeObs]
        group <- group[completeObs]
    }

stor1 <- stor2 <- NULL

if (method == "SW") {
        for (i in 1:k) {
        kk = shapiro.test(y[which(group == (levels(group)[i]))])
        stor1 = c(stor1, kk$statistic)
        stor2 = c(stor2, kk$p)
    }
method.name = "Shapiro-Wilk Normality Test"
}

if (method == "SF") {
        for (i in 1:k) {
        kk = sf.test(y[which(group == (levels(group)[i]))])
        stor1 = c(stor1, kk$statistic)
        stor2 = c(stor2, kk$p)
    }
method.name = "Shapiro-Francia Normality Test"
}

if (method == "KS") {
        for (i in 1:k) {
        kk = lillie.test(y[which(group == (levels(group)[i]))])
        stor1 = c(stor1, kk$statistic)
        stor2 = c(stor2, kk$p)
    }
method.name = "Kolmogorov-Smirnov Normality Test"
}

if (method == "AD") {
        for (i in 1:k) {
        kk = ad.test(y[which(group == (levels(group)[i]))])
        stor1 = c(stor1, kk$statistic)
        stor2 = c(stor2, kk$p)
    }
method.name = "Anderson-Darling Normality Test"
}

if (method == "CVM") {
        for (i in 1:k) {
        kk = cvm.test(y[which(group == (levels(group)[i]))])
        stor1 = c(stor1, kk$statistic)
        stor2 = c(stor2, kk$p)
    }
method.name = "Cramer-von Mises Normality Test"
}

if (method == "PT") {
        for (i in 1:k) {
        kk = pearson.test(y[which(group == (levels(group)[i]))])
        stor1 = c(stor1, kk$statistic)
        stor2 = c(stor2, kk$p)
    }
method.name = "Pearson Chi-square Normality Test"
}

   store = data.frame(matrix(NA, nrow = k, ncol = 4))
    colnames(store) = c("Level", "Statistic", "p.value", "Normality")
    store$Statistic = stor1
    store$p.value = stor2
    store$Normality = ifelse(store$p.value > alpha, "YES", "NO")
    store$Level = levels(group)

 if (verbose) {
        cat("\n", "",method.name, "\n", sep = " ")
        cat("---------------------------------------------", 
            "\n", sep = " ")
        cat("  data :", dname1, "and", dname2, "\n\n", sep = " ")
          
        print(store)
          cat("---------------------------------------------", 
            "\n\n", sep = " ")
    }

    invisible(store)

if (!is.null(plot)){
plot = match.arg(plot)

if (plot == "qqplot") {

if (is.null(mfrow)) par(mfrow = c(ceiling(k/ceiling(sqrt(k))), ceiling(sqrt(k)))) else par(mfrow = mfrow)

            for (i in 1:k) {
                qqnorm(y[which(group == (levels(group)[i]))], main = paste("Normal Q-Q Plot (", levels(group)[i], ")", sep = ""))
                qqline(y[which(group == (levels(group)[i]))])
            }
}

if (plot == "histogram") {

if (is.null(mfrow)) par(mfrow = c(ceiling(k/ceiling(sqrt(k))), ceiling(sqrt(k)))) else par(mfrow = mfrow)

       for (i in 1:k) {
       hist(y[which(group == (levels(group)[i]))], xlab = levels(group)[i], freq = FALSE, main = paste("Histogram of", levels(group)[i]))
	  x<-NULL
       rm(x)
       curve(dnorm(x, mean = mean(y[which(group == (levels(group)[i]))]), sd = sd(y[which(group == (levels(group)[i]))])), col = "red", add = TRUE)
       }
}

if (plot == "qqplot-histogram"){

par(mfrow = c(2,k))

		for (i in 1:k) {
            qqnorm(y[which(group == (levels(group)[i]))], main = paste("Normal Q-Q Plot (", levels(group)[i], ")", sep = ""))
            qqline(y[which(group == (levels(group)[i]))])
            }

 		for (i in (1):(k)) {
       	hist(y[which(group == (levels(group)[i]))], xlab = levels(group)[i], freq = FALSE, main = paste("Histogram of", levels(group)[i]))
		x<-NULL
		rm(x)
       	curve(dnorm(x, mean = mean(y[which(group == (levels(group)[i]))]), sd = sd(y[which(group == (levels(group)[i]))])), col = "red", add = TRUE)
       	}

}
}


}

