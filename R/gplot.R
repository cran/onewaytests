gplot<- function(formula, data, type = c("boxplot-violin", "boxplot", "violin", "errorbar"), width = c(0.30, 1.0, 0.2), 
                  dots = TRUE, binwidth = 0.05, color_manual = NULL, theme = theme_bw(), xlab = NULL, ylab = NULL, 
                  title = NULL, option = c("sd", "se"), bar = FALSE, na.rm = TRUE){

data <- model.frame(formula, data)
fml <- as.character(formula)
ftmp <- strsplit(fml, "~")
y <- as.vector(ftmp[[2]])
Factors <- strsplit(ftmp[[3]][1], "[*]")[[1]]
FacA <- strsplit(Factors[1], " ")[[1]][1]

dname1 <- y
dname2 <- FacA

level_<-nlevels(data[, FacA])

if (!is.data.frame(data)) stop("Data must be in data.frame class.")
if (length(Factors) != 1) stop("Please correct the RHS of the formula. Formula must include one factor.")
if (!is.factor(data[, colnames(data) == FacA])) stop(paste(FacA, "must be a factor."))
if (!is.numeric(data[, colnames(data) == y])) stop(paste(y, "must be a numeric."))
  
if (na.rm) {
    completeObs <- complete.cases(data)
    data <- data[completeObs, ]
}
  

y_vector = data[[y]]
FacA_vector = data[[FacA]]
data <- as.data.frame(cbind(y_vector, FacA_vector))
data$FacA_vector <- as.factor(FacA_vector)

type = match.arg(type)
  
if (type == "boxplot") {

out <- ggplot(data, aes(x = FacA_vector, y = y_vector, fill = FacA_vector))+ geom_boxplot(width = width[1])
							
}else if(type == "errorbar"){

option <- match.arg(option)
Extremity <- dose <- dev <- NULL
colnames(data) <- c(y, FacA)
result <- describe(formula = formula, data = data)
df2 <- data.frame(matrix(0, dim(result)[1], 3))
colnames(df2) <- c("groups", "mean", "dev")
df2$groups <- rownames(result)
df2$mean <- result[, 2]



if (option == "se") df2$dev <- result[, 3]/sqrt(result[, 1])
if (option == "sd") df2$dev <- result[, 3]
    
out <- ggplot(df2, aes(x = groups, y = mean, fill = groups)) 

if (bar == "TRUE"){
out <- out + geom_bar(stat = "identity", color = "black", position = position_dodge(), linewidth=1) 
}

out <- out + geom_point() + geom_errorbar(aes(ymin =mean - dev , ymax = mean + dev), width = width[3], linewidth = 0.8, position = position_dodge(0.9))
  
}else if(type == "violin"){
    
out <- ggplot(data, aes(x = FacA_vector, y = y_vector, fill = FacA_vector))+ geom_violin(width = width[2])
  
}else if(type == "boxplot-violin"){

out <- ggplot(data, aes(x = FacA_vector, y = y_vector, fill = FacA_vector))+ geom_violin(width = width[2])

out <- out + geom_boxplot(width = width[1], fill = "white")

}


if (dots) out <- out + geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(1), dotsize=1, binwidth = binwidth)




if (is.null(theme)){ theme <- theme_bw()
}else{ theme <- theme
}

out <- out + theme


if(is.null(color_manual)){ a <- wes_palette("FantasticFox1", n = level_)
}else{ a <- color_manual
}

out <- out+ scale_fill_manual(values = a)
  
if (is.null(ylab)){ y.name <- dname1
}else{ y.name <- ylab
}
  
if (is.null(xlab)){ x.name <- dname2
}else{ x.name <- xlab
}
  
if (is.null(title)){ title.name <- "" 
}else{ title.name <- title
}
  
out <- out + labs(x = x.name, y = y.name, title = title.name)
out <- out + theme(legend.position = "none", plot.title = element_text(size=11))

return(out)
}