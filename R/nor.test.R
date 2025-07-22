
nor.test <- function(formula, data, method = c("SW", "SF", "LT", "AD", "CVM", "PT"), plot = c("qqplot-histogram", "qqplot", "histogram"), mfrow = NULL, alpha = 0.05, na.rm = TRUE, verbose = TRUE){ 

  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

if (any(colnames(data)==dp[[3L]])==FALSE) stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
if (any(colnames(data)==dp[[2L]])==FALSE) stop("The name of response variable does not match the variable names in the data.")

y = data[[dp[[2L]]]]
group = data[[dp[[3L]]]]


if (!(is.factor(group)|is.character(group))) stop("The group variable must be a factor or a character.") 
if (is.character(group)) group <- as.factor(group)
if (!is.numeric(y)) stop("The response must be a numeric variable.") 

  dname1<-dp[[2L]]
  dname2<-dp[[3L]]

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


if (method == "SW") {
 kk <- tapply(y, group, shapiro.test)
 stor1 <- as.numeric(unlist(lapply(1:k, function(i) kk[[i]]$statistic)))
 stor2 <- as.numeric(unlist(lapply(1:k, function(i) kk[[i]]$p)))
 method.name = "Shapiro-Wilk Normality Test"
}

if (method == "SF") {
  kk <- tapply(y, group, sf.test)
  stor1 <- as.numeric(unlist(lapply(1:k, function(i) kk[[i]]$statistic)))
  stor2 <- as.numeric(unlist(lapply(1:k, function(i) kk[[i]]$p)))
  method.name = "Shapiro-Francia Normality Test"
}

if (method == "LT") {
  kk <- tapply(y, group, lillie.test)
  stor1 <- as.numeric(unlist(lapply(1:k, function(i) kk[[i]]$statistic)))
  stor2 <- as.numeric(unlist(lapply(1:k, function(i) kk[[i]]$p)))
  method.name = "Lilliefors (Kolmogorov-Smirnov) Normality Test"
}

if (method == "AD") {
  kk <- tapply(y, group, ad.test)
  stor1 <- as.numeric(unlist(lapply(1:k, function(i) kk[[i]]$statistic)))
  stor2 <- as.numeric(unlist(lapply(1:k, function(i) kk[[i]]$p)))
  method.name = "Anderson-Darling Normality Test"
}

if (method == "CVM") {
  kk <- tapply(y, group, cvm.test)
  stor1 <- as.numeric(unlist(lapply(1:k, function(i) kk[[i]]$statistic)))
  stor2 <- as.numeric(unlist(lapply(1:k, function(i) kk[[i]]$p)))
  method.name = "Cramer-von Mises Normality Test"
}

if (method == "PT") {
  kk <- tapply(y, group, pearson.test)
  stor1 <- as.numeric(unlist(lapply(1:k, function(i) kk[[i]]$statistic)))
  stor2 <- as.numeric(unlist(lapply(1:k, function(i) kk[[i]]$p)))
  method.name = "Pearson Chi-square Normality Test"
}

   store = data.frame(matrix(NA, nrow = k, ncol = 4))
    store$X1 = levels(group)
    store$X2 = stor1
    store$X3 = stor2
    store$X4 = ifelse(store$X3 > alpha, "Not reject", "Reject")
    colnames(store) = c("Level", "Statistic", "p.value", "  Normality")


 if (verbose) {
        cat("\n", "",method.name, paste("(alpha = ",alpha,")",sep = ""), "\n", sep = " ")
        cat("--------------------------------------------------", 
            "\n", sep = " ")
        cat("  dependent var :", dp[[2L]], "\n", sep = " ")
        cat("  grouping var  :", dp[[3L]], "\n\n", sep = " ")
          
        print(store)
          cat("--------------------------------------------------", 
            "\n\n", sep = " ")
    }

    

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

invisible(store)
}

