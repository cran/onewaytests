\name{nor.test}
\alias{nor.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normality Tests
}
\description{
\code{nor.test} performs normality tests including Shapiro-Wilk, Shapiro-Francia, Kolmogorov-Smirnov, Anderson-Darling, Cramer-von Mises, Pearson Chi-square tests, and also assess the normality of each group through plots.
}

\usage{nor.test(formula, data, method = c("SW", "SF", "LT", "AD", "CVM", "PT"), 
  alpha = 0.05, plot = c("qqplot-histogram", "qqplot", "histogram"), mfrow = NULL, 
  na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{method}{a character string to select one of the normality tests. "SW": Shapiro-Wilk test, "SF": Shapiro-Francia test, "LT": Lilliefors (Kolmogorov-Smirnov) test, "AD": Anderson-Darling test, "CVM": Cramer-von Mises test, "PT": Pearson Chi-square test.} 
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

library(onewaytests)

nor.test(Sepal.Length ~ Species, data = iris, method = "SW", plot = "qqplot-histogram")
nor.test(Sepal.Length ~ Species, data = iris, method = "SF", plot = "qqplot", mfrow = c(1,3))

}

\keyword{functions}
