
welch.test <- function(formula, data, rate = 0, alpha = 0.05, na.rm = TRUE, verbose = TRUE) {

  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

if (rate==0){METHOD <- "Welch's Heteroscedastic F Test"
}else{METHOD <- "Welch's Heteroscedastic F Test with Trimmed Means and Winsorized Variances"}

 if (na.rm){
    completeObs <- complete.cases(data)
    data <- data[completeObs,]
  }

if (any(colnames(data)==dp[[3L]])==FALSE) stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
if (any(colnames(data)==dp[[2L]])==FALSE) stop("The name of response variable does not match the variable names in the data.")

y=data[[dp[[2L]]]]
group=as.factor(data[[dp[[3L]]]])


if (!(is.factor(group)|is.character(group))) stop("The group variable must be a factor or a character.") 
if (is.character(group)) group <- as.factor(group)
if (!is.numeric(y)) stop("The response must be a numeric variable.") 

trim=function(x,rate){
n=length(x)
xx=sort(x)
lambda=round(n*rate)
xx[(lambda+1):(n-lambda)]
}

wins=function(x,rate){
n=length(x)
xx=sort(x)
lambda=round(n*rate)
xxx=c(rep(xx[lambda+1],lambda),xx[(lambda+1):(n-lambda)],rep(xx[n-lambda],lambda))
xxx
}

n <- length(y)
x.levels <- levels(factor(group))
y.vars <- y.means <-lambda<- m <- y.n <- w <- b <- q <-NULL

for (i in x.levels) {

y.n[i] <- length(y[group==i])

lambda[i]=round(y.n[i]*rate)

b[i]=y.n[i]-2*lambda[i]

y.vars[i] <- var(wins(y[group==i],rate))

q[i]=(y.n[i]-1)*y.vars[i]/b[i]/(b[i]-1)

w[i] <- 1/q[i]

y.means[i] <- mean(trim(y[group==i],rate))
  
}

U=sum(w)

w_y=sum(w*y.means)/U

J=length(x.levels)

A=sum(w*(y.means-w_y)^2)/(J-1)

B=2*(J-2)/(J^2-1)*sum((1-w/U)^2/(b-1))



Ftest=A/(B+1)

df1=J-1
df2=(3/(J^2-1)*sum((1-w/U)^2/(b-1)))^(-1)


p.value=pf(Ftest,df1,df2,lower.tail = F)



if (verbose) {
            cat("\n", "",METHOD, paste("(alpha = ",alpha,")",sep = ""), "\n", 
                sep = " ")
            cat(if (rate!=0){"----------------------------------------------------------------------------------------------"}else{"-------------------------------------------------------------"}, 
                "\n", sep = " ")
            cat("  data :", DNAME, "\n\n", sep = " ")
            cat("  statistic  :", Ftest, "\n", sep = " ")
            cat("  num df     :", df1, "\n", sep = " ")
		 cat("  denom df   :", df2, "\n", sep = " ")
		 cat("  p.value    :", p.value, "\n\n", sep = " ")
            cat(if (p.value > alpha) {
                "  Result     : Difference is not statistically significant."
            }
            else {
                "  Result     : Difference is statistically significant."
            }, "\n")
            cat(if (rate!=0){"----------------------------------------------------------------------------------------------"}else{"-------------------------------------------------------------"}, 
                "\n", sep = " ")
        }

result <- list()
result$statistic <- Ftest
result$parameter <- c(df1,df2)
result$p.value <- p.value
result$alpha <- alpha
result$method <- METHOD 
result$rate <- rate
result$data <- data
result$formula <- formula


attr(result, "class") <- "owt"
invisible(result)

}

