\name{johansen.test}
\alias{johansen.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Johansen F Test
}
\description{
\code{johansen.test} performs Johansen F test. This test is an alternative to one-way ANOVA when variances are homogeneous. The test statistic follows an F distribution.
}

\usage{johansen.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{alpha}{the level of significance to assess the statistical difference. Default is
set to alpha = 0.05.} 
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with class "owt" containing the following components:

\item{statistic}{the Johansen F test statistic.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Johansen F Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{
Johansen, S. (1980). The Welch-James Approximation to the Distribution of the Residual Sum of Squares in a Weighted Linear Regression, \emph{Biometrika}, \bold{67:1}, 58-92. }


\author{
Osman Dag}



\examples{

library(onewaytests)

johansen.test(Sepal.Length ~ Species, data = iris)

out <- johansen.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
paircomp(out)


}

\keyword{functions}
