
describe <- function(formula, data){

  dp=as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])

if (any(colnames(data)==dp[[3L]])==FALSE) stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
if (any(colnames(data)==dp[[2L]])==FALSE) stop("The name of response variable does not match the variable names in the data.")

y = data[[dp[[2L]]]]
group = data[[dp[[3L]]]]


if (!(is.factor(group)|is.character(group))) stop("The group variable must be a factor or a character.") 
if (is.character(group)) group <- as.factor(group)
if (!is.numeric(y)) stop("The response must be a numeric variable.") 


if (length(y)!=length(group)) stop("Lengths of y and group variables are not equal.")


x.levels <- levels(factor(group))

y.NAs <- NULL
for (i in x.levels) {
	  y.NAs[i] <- length(y[group == i][is.na(y[group == i])])
       
    }
completeObs <- complete.cases(y)
y <- y[completeObs]
group <- group[completeObs]

y.skew <- y.kurtosis <-y.means <- y.n <- y.medians <- y.firstqs <- y.thirdqs <- y.mins <- y.maxs <- y.sds <- NULL
for (i in x.levels) {	 
        y.means[i] <- mean(y[group == i])
        y.n[i] <- length(y[group == i])
	  y.medians[i] <- median(y[group == i])
	  y.firstqs[i] <- as.numeric(quantile(y[group == i])[2])
	  y.thirdqs[i] <- as.numeric(quantile(y[group == i])[4])
 	  y.mins[i] <- min(y[group == i])
        y.maxs[i] <- max(y[group == i])
        y.sds[i] <- sd(y[group == i])
        y.skew[i] <- skewness(y[group == i])
        y.kurtosis[i] <- kurtosis(y[group == i])

    }
out=as.data.frame(cbind(y.n,y.means,y.sds,y.medians,y.mins,y.maxs,y.firstqs,y.thirdqs,y.skew,y.kurtosis,y.NAs))


colnames(out) = c("n", "Mean", "Std.Dev", "Median", "Min", "Max", "25th","75th","Skewness","Kurtosis","NA")
return(out) 

}



