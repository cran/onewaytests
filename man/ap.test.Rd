\name{ap.test}
\alias{ap.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Approximate F Test
}
\description{
\code{ap.test} performs approximate F test.
}

\usage{ap.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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

\item{statistic}{the approximate F test statistic.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Approximate F Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{
Asiribo, O., Gurland, J. (1990). Coping with Variance Heterogeneity. \emph{Communications in Statistics-Theory and Methods}, \bold{19:11}, 4029-4048.
}


\author{
Merve Kasikci
}



\examples{

library(onewaytests)

ap.test(Sepal.Length ~ Species, data = iris)

out <- ap.test(Sepal.Length ~ Species, data = iris)
paircomp(out)


}

\keyword{functions}
