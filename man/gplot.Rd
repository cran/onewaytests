\name{gplot}
\alias{gplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Box-and-Whisker, Violin Plots and Error Bars
}
\description{
\code{gplot} produce box-and-whisker plots, violin plots, and error bars of the given grouped values.
}

\usage{gplot(formula, data, type = c("boxplot-violin", "boxplot", "violin", "errorbar"), 
  width = c(0.3, 1.0, 0.2), dots = TRUE, binwidth = 0.05, color_manual = NULL, 
  theme = theme_bw(), xlab = NULL, ylab = NULL, title = NULL, 
  option = c("sd", "se"), bar = FALSE, na.rm = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{type}{a character string to select one of the plots. "boxplot-violin": box-and-whisker plot with violin lines, "boxplot": box-and-whisker plot, "violin": violin plot, "errorbar": error bar.} 
  \item{width}{a vector including three numeric values. First numeric represents the width of the boxes for box-and-whisker plots (defaults to 0.3). Second numeric belongs to the width of violin plot (defaults to 1.0). Third numeric represents the width of the little lines at the tops and bottoms of the error bars (defaults to 0.20).} 
  \item{dots}{a logical to draw the dots corresponding the data values.} 
  \item{binwidth}{a numeric to specify bin width of dot(s), defaults to 0.05.} 
  \item{color_manual}{a vector of colors. A palette can also be defined with \code{\link[wesanderson]{wes_palette}}. Default is set to "FantasticFox1" available in \code{\link[wesanderson]{wes_palette}}.} 
  \item{theme}{a theme (see \code{\link[ggplot2]{ggtheme}}). Default is set to theme_bw().} 
  \item{xlab}{a label for the x axis, defaults to a description of x.} 
  \item{ylab}{a label for the y axis, defaults to a description of y.} 
  \item{title}{a main title for the plot.} 
  \item{option}{a character string to select one of the options to draw error bars with standard error or standard deviation. "se": standard error, "sd": standard deviation. Defaults to "sd".} 
  \item{bar}{a logical to add bar to errorbars. Default is fixed to bar = FALSE.} 
  \item{na.rm}{a logical indicating whether NA values should be stripped before the computation proceeds.}

}

\details{

The upper whisker of box-and-whisker plots extends from the hinge to the highest value that is within 1.5 * IQR of the hinge, where IQR is the inter-quartile range. The lower whisker extends from the hinge to the lowest value within 1.5 * IQR of the hinge. Data out of the ends of the whiskers are outliers and plotted as points.

}


\author{
Osman Dag
}

\seealso{
\code{\link[ggplot2]{geom_boxplot}} \code{\link[ggplot2]{geom_violin}}  

 
}


\examples{

library(onewaytests)

# box-and-whisker with dots
gplot(Sepal.Length~Species, data = iris, type = "boxplot") 

# box-and-whisker without dots
gplot(Sepal.Length~Species, data = iris, type = "boxplot", dots = FALSE) 

# to change the width of the boxes for box-and-whisker plots
gplot(Sepal.Length~Species, data = iris, type = "boxplot", width = c(0.4, NA, NA)) 

# violin plot with dots
gplot(Sepal.Length~Species, data = iris, type = "violin") 

# to change the width of violin plots
gplot(Sepal.Length~Species, data = iris, type = "violin", width = c(NA, 0.8, NA)) 

# box-and-whisker plot with violin lines and dots
gplot(Sepal.Length~Species, data = iris, type = "boxplot-violin") 

# to change the width of the boxes for box-and-whisker plots and the width of violin plots
gplot(Sepal.Length~Species, data = iris, type = "boxplot-violin", width = c(0.25, 0.95, NA)) 

# to change the theme
library(ggplot2)
gplot(Sepal.Length~Species, data = iris, type = "boxplot-violin", width = c(0.25, 0.95, NA), 
theme = theme_minimal()) 

# to specify the colors 
gplot(Sepal.Length~Species, data = iris, type = "boxplot-violin", width = c(0.25, 0.95, NA), 
color_manual=c("#999999","#E69F00","#56B4E9")) 

# to specify the colors as white
gplot(Sepal.Length~Species, data = iris, type = "boxplot-violin", width = c(0.25, 0.95, NA), 
color_manual=c("white","white","white"))

#to change color palette 
library(wesanderson) 
gplot(Sepal.Length~Species, data = iris, type = "boxplot-violin", width = c(0.25, 0.95, NA), 
color_manual=wes_palette(name="GrandBudapest1",n=3))

# error bars (mean +- standard deviation) without bars
gplot(Sepal.Length~Species, data = iris, type = "errorbar", option = "sd", bar = FALSE) 

# error bars (mean +- standard deviation) with bars
gplot(Sepal.Length~Species, data = iris, type = "errorbar", option = "sd", bar = TRUE) 

# to change the width of the little lines at the tops and bottoms of the error bars
gplot(Sepal.Length~Species, data = iris, type = "errorbar", width = c(NA, NA, 0.25)) 

# error bars (mean +- standard error) without bars
gplot(Sepal.Length~Species, data = iris, type = "errorbar", option = "se", bar = FALSE) 


}

\keyword{functions}
