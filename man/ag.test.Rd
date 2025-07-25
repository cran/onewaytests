\name{ag.test}
\alias{ag.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alexander-Govern Test
}
\description{
\code{ag.test} performs Alexander-Govern test. This test is an alternative to one-way ANOVA when group variances are not homogeneous. The test statistic follows a chi-square distribution.
}

\usage{ag.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{alpha}{the level of significance to assess the statistical difference. Default is
set to alpha = 0.05.} 
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}


}

%\note{
%}


\value{
A list with class "owt" containing the following components:
\item{statistic}{the Alexander-Govern test statistic.}
\item{parameter}{the parameter(s) of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Alexander-Govern Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Dag, O., Dolgun, A., Konar, N.M. (2018). onewaytests: An R Package for One-Way Tests in Independent Groups Designs. \emph{The R Journal}, \bold{10:1}, 175-199.

Schneider, P. J., Penfield, D. A. (1997). Alexander and Govern's Approximation: Providing an Alternative to ANOVA Under Variance Heterogeneity. \emph{The Journal of Experimental Education}, \bold{65:3}, 271-286.

}


\author{
Osman Dag
}



\examples{

######

library(onewaytests)

ag.test(Sepal.Length ~ Species, data = iris)

out <- ag.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
paircomp(out)

######

library(onewaytests)
library(tibble)

iris <- as_tibble(iris)
ag.test(Sepal.Length ~ Species, data = iris)

out <- ag.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
paircomp(out)

######
}

\keyword{functions}
