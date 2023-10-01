\name{b2.test}
\alias{b2.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
B Square Test
}
\description{
\code{b2.test} performs B square test.
}

\usage{b2.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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
\item{statistic}{the B square test statistic.}
\item{parameter}{the parameter(s) of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "B Square Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}

\references{

Ozdemir, A.F., Kurt, S. (2006). One Way Fixed Effect Analysis of Variance under Variance Heterogeneity and a Solution Proposal. \emph{Selcuk Journal of Applied Mathematics}, \bold{7:2}, 81-90.

}

\author{
Merve Kasikci
}

\examples{

library(onewaytests)

b2.test(Sepal.Length ~ Species, data = iris)

out <- b2.test(Sepal.Length ~ Species, data = iris)
paircomp(out)

}

\keyword{functions}
