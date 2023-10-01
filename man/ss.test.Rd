\name{ss.test}
\alias{ss.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scott-Smith Test
}
\description{
\code{ss.test} performs Scott-Smith test.
}

\usage{ss.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{alpha}{the level of significance to assess the statistical difference. Default is
set to alpha = 0.05.} 
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}


}



\value{
A list with class "owt" containing the following components:
\item{statistic}{the Scott-Smith test statistic.}
\item{parameter}{the parameter(s) of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Scott-Smith Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Scott, A., Smith, T. (1971). Interval Estimates for Linear Combinations of Means. \emph{Journal of the Royal Statistical Society: Series C (Applied Statistics)}, \bold{20:3}, 276-285.

}


\author{
Osman Dag
}



\examples{

library(onewaytests)

ss.test(Sepal.Length ~ Species, data = iris)

out <- ss.test(Sepal.Length ~ Species, data = iris)
paircomp(out)


}

\keyword{functions}
