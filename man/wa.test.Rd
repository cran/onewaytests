\name{wa.test}
\alias{wa.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Welch-Aspin Test
}
\description{
\code{wa.test} performs Welch-Aspin test. This test is a modification of Welch test. The test statistic follows an F distribution.
}

\usage{wa.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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

\item{statistic}{the Welch-Aspin test statistic.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Welch-Aspin Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Aspin, A.A. (1948). An Examination and Further Development of a Formula Arising in the Problem of Comparing Two Mean Values. \emph{Biometrika}, \bold{35:1/2}, 88-96.

}


\author{
Osman Dag
}



\examples{

library(onewaytests)

wa.test(Sepal.Length ~ Species, data = iris)

out <- wa.test(Sepal.Length ~ Species, data = iris)
summary(out)
paircomp(out)

}

\keyword{functions}
