
describe <- function(formula, data){
  
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
  
  
  if (length(y)!=length(group)) stop("Lengths of y and group variables are not equal.")
  
  x.levels <- levels(factor(group))
  
  NAs <- function(y){
    length(y[is.na(y)])
  }
  
  y.NAs <- tapply(y, group, NAs)
  completeObs <- complete.cases(y)
  y <- y[completeObs]
  group <- group[completeObs]
  
  y.means <- tapply(y, group, mean)
  y.n <- tapply(y, group, length)
  y.medians <- tapply(y, group, median)
  y.firstqs <- tapply(y, group, quantile, p=0.25)
  y.thirdqs <- tapply(y, group, quantile, p=0.75)
  y.mins <- tapply(y, group, min)
  y.maxs <- tapply(y, group, max)
  y.sds <- tapply(y, group, sd)
  y.skew <- tapply(y, group, skewness)
  y.kurtosis <- tapply(y, group, kurtosis)
  
  out=as.data.frame(cbind(y.n,y.means,y.sds,y.medians,y.mins,y.maxs,y.firstqs,y.thirdqs,y.skew,y.kurtosis,y.NAs))
  
  
  colnames(out) = c("n", "Mean", "Std.Dev", "Median", "Min", "Max", "25th","75th","Skewness","Kurtosis","NA")
  return(out) 
  
}



