ag.test <- function(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {


  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

  METHOD <- "Alexander-Govern Test"

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



  n <- length(y)
  x.levels <- levels(factor(group))
  y.se <- w <- y.means <- y.n <- NULL
  for (i in x.levels) {
   
    y.se[i] <- sqrt(var(y[group==i])/length(y[group==i]))
 
    y.means[i] <- mean(y[group==i])
 
    y.n[i] <- length(y[group==i])
  }

  w <- (1/y.se^2)/sum(1/y.se^2)


  y.plus <- sum(w * y.means)


  t <- (y.means - y.plus)/y.se


  a <- y.n - 1.5
  b <- 48*a^2
  c <- (a * log(1 + t^2/(y.n-1)))^(.5)
  z <- c + (c^3+3*c)/b - (4*c^7+33*c^5+240*c^3+855*c)/(10*b^2+8*b*c^4+1000*b)


  approx <- sum(z^2)
  df <- length(x.levels)-1
  p.value <- pchisq(approx, df=df, lower.tail=FALSE)

  

if (verbose) {
            cat("\n", "",METHOD, paste("(alpha = ",alpha,")",sep = ""), "\n", 
                sep = " ")
            cat("-------------------------------------------------------------", 
                "\n", sep = " ")
            cat("  data :", DNAME, "\n\n", sep = " ")
            cat("  statistic  :", approx, "\n", sep = " ")
            cat("  parameter  :", df, "\n", sep = " ")
		 cat("  p.value    :", p.value, "\n\n", sep = " ")
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
result$statistic <- approx
result$parameter <- df
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD 
result$data <- data
result$formula <- formula



attr(result, "class") <- "owt"
invisible(result)

  



}
