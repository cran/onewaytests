\name{gplot}
\alias{gplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Box-and-Whisker Plots and Error Bars
}
\description{
\code{gplot} produce box-and-whisker plots and error bars of the given grouped values.
}

\usage{gplot(formula, data, type = c("boxplot", "errorbar"), violin = TRUE, xlab = NULL, 
  ylab = NULL, title = NULL, width = NULL, option = c("se", "sd"), na.rm = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a data frame containing the variables in the formula \code{formula}} 
  \item{type}{a character string to select one of the plots. "boxplot": box-and-whisker plot, "errorbar": error bar.} 
  \item{violin}{a logical adding violin plot on box-and-whisker plot.}
  \item{xlab}{a label for the x axis, defaults to a description of x.} 
  \item{ylab}{a label for the y axis, defaults to a description of y.} 
  \item{title}{a main title for the plot.} 
  \item{width}{a numeric giving the width of the boxes for box-and-whisker plots (defaults to 0.4) and the width of the little lines at the tops and bottoms of the error bars (defaults to 0.15).} 
  \item{option}{a character string to select one of the options to draw error bars with standard error or standard deviation. "se": standard error, "sd": standard deviation. Defaults to "se".} 
  \item{na.rm}{a logical indicating whether NA values should be stripped before the computation proceeds.}

}

\details{

The upper whisker of box-and-whisker plots extends from the hinge to the highest value that is within 1.5 * IQR of the hinge, where IQR is the inter-quartile range. The lower whisker extends from the hinge to the lowest value within 1.5 * IQR of the hinge. Data out of the ends of the whiskers are outliers and plotted as points.

}


\author{
Osman Dag
}

\seealso{
\code{\link[ggplot2]{geom_boxplot}} \code{\link[ggplot2]{geom_errorbar}} \code{\link[ggplot2]{geom_violin}}  

 
}


\examples{

gplot(Sepal.Length ~ Species, data = iris, type = "boxplot")
gplot(Sepal.Length ~ Species, data = iris, type = "boxplot", violin = FALSE)
gplot(Sepal.Length ~ Species, data = iris, type = "errorbar", option = "se")
gplot(Sepal.Length ~ Species, data = iris, type = "errorbar", option = "sd")

}

\keyword{functions}
