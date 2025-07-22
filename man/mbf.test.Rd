\name{mbf.test}
\alias{mbf.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modified Brown-Forsythe Test
}
\description{
\code{mbf.test} performs modified Brown-Forsythe test. This test is a modification of Brown-Forsythe test to overcome the problem of higher than acceptable rate of false positives. The test statistic follows an F distribution.
}

\usage{mbf.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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

\item{statistic}{the modified Brown-Forsythe test statistic.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Modified Brown-Forsythe Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Mehrotra, D.V. (1997). Improving the Brown-Forsythe Solution to the Generalized Behrens-Fisher Problem. \emph{Communications in Statistics-Simulation and Computation}, \bold{26:3}, 1139-1145.

}


\author{
Merve Kasikci
}



\examples{

library(onewaytests)

mbf.test(Sepal.Length ~ Species, data = iris)

out <- mbf.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
paircomp(out)

}

\keyword{functions}
