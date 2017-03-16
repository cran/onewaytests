\name{nor.test}
\alias{nor.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normality Tests
}
\description{
\code{nor.test} performs normality tests including Shapiro-Wilk, Shapiro-Francia, Kolmogorov-Smirnov, Anderson-Darling, Cramer-von Mises, Pearson Chi-square tests, and also assess the normality of each group through plots.
}

\usage{nor.test(y, group, method = c("SW", "SF", "KS", "AD", "CVM", "PT"),  alpha = 0.05, 
  plot = c("qqplot-histogram", "qqplot", "histogram"), mfrow = NULL, na.rm = TRUE,
  verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a numeric vector of data values.}
  \item{group}{a vector or factor object giving the group for the corresponding elements of y.} 
  \item{method}{a character string to select one of the normality tests. "SW": Shapiro-Wilk test, "SF": Shapiro-Francia test, "KS": Komogorov-Smirnov test, "AD": Anderson-Darling test, "CVM": Cramer-von Mises test, "PT": Pearson Chi-square test.} 
  \item{alpha}{the level of significance to assess normality. Default is set to alpha = 0.05.}
  \item{plot}{a character string to select one of the plots including qqplot-histogram, qqplot, histogram. The red line is the density line of normal distribution.}
  \item{mfrow}{a two element vector to draw subsequent figures.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A data frame gives the test results for the normality of groups via corresponding normality.

}


\author{
Osman Dag
}

\seealso{
\code{\link[onewaytests]{homog.test}} \code{\link[onewaytests]{gplot}} \code{\link[stats]{shapiro.test}} 
 
}



\examples{

nor.test(iris$Sepal.Width, iris$Species, method = "SW", plot = "qqplot-histogram")
nor.test(iris$Sepal.Width, iris$Species, method = "SF", plot = "qqplot", mfrow = c(1,3))

}

\keyword{functions}
