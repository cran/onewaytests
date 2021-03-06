gplot <- function (formula, data, type = c("boxplot", "errorbar"), violin = TRUE, xlab = NULL, ylab = NULL, title = NULL, width = NULL, option = c("se", "sd"), na.rm = TRUE){

data <- model.frame(formula, data)
dp <- as.character(formula)
DNAME <- paste(dp[[2L]], "and", dp[[3L]])

dname1<-dp[[2L]]
dname2<-dp[[3L]]

if (any(colnames(data)==dp[[3L]])==FALSE) stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
if (any(colnames(data)==dp[[2L]])==FALSE) stop("The name of response variable does not match the variable names in the data.")

y = data[[dp[[2L]]]]
group = data[[dp[[3L]]]]


if (!(is.factor(group)|is.character(group))) stop("The group variable must be a factor or a character.") 
if (is.character(group)) group <- as.factor(group)
if (!is.numeric(y)) stop("The response must be a numeric variable.") 

if (na.rm) {
        completeObs <- complete.cases(y, group)
        y <- y[completeObs]
        group <- group[completeObs]
    }
group <- as.factor(group)
type = match.arg(type)

if (type == "boxplot"){

data<-as.data.frame(cbind(y,group))
data$group <- as.factor(group)
if (is.null(width)) width <- 0.3 else width <- width

out <- ggplot(data, aes(group, y))
if (violin==TRUE) out <- out + geom_violin() 
out<-out + geom_boxplot(width = width)

}

if (type == "errorbar"){
option = match.arg(option)
trt<-resp<-dev<-NULL

if (option=="se") dev=tapply(y,group,sd)/sqrt(tapply(y,group,length)) 
if (option=="sd") dev=tapply(y,group,sd)

df <- data.frame(
  trt = levels(group),
  resp = tapply(y,group,mean),
  dev)

limits <- aes(ymax = resp + dev, ymin=resp - dev)

if (is.null(width)) width <- 0.15 else width <- width

out <- ggplot(df, aes(y=resp, x=trt))
out <- out + geom_point() + geom_errorbar(limits, width = width, size = 0.8)

}

if (is.null(ylab)) out<-out+ylab(dname1) else out<-out+ylab(ylab)
if (is.null(xlab)) out<-out+xlab(dname2) else out<-out+xlab(xlab)
if (is.null(title)) out<-out+ggtitle("") else out<-out+ggtitle(title)

return(out)
}
