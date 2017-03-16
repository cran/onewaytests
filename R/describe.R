
describe <- function(y, group=NULL){


if (is.null(group)){
dname <- deparse(substitute(y))
y.NAs <- length(y[is.na(y)])
completeObs <- complete.cases(y)
y <- y[completeObs]
y.means <- mean(y)
y.n <- length(y)
y.medians <- median(y)
y.firstqs <- as.numeric(quantile(y)[2])
y.thirdqs <- as.numeric(quantile(y)[4])
y.mins <- min(y)
y.maxs<- max(y)
y.sds <- sd(y)
y.skew<- skewness(y)
y.kurtosis <- kurtosis(y)
out=as.data.frame(cbind(y.n,y.means,y.sds,y.medians,y.mins,y.maxs,y.firstqs,y.thirdqs,y.skew,y.kurtosis,y.NAs))

rownames(out)<-dname
}else{

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

}
colnames(out) = c("n", "Mean", "Std.Dev", "Median", "Min", "Max", "25th","75th","Skewness","Kurtosis","NA")
return(out) 

}



