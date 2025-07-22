\name{cochran.test}
\alias{cochran.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cochran Test
}
\description{
\code{cochran.test} performs Cochran test. This test is a heteroscedastic alternative to one-way ANOVA. The test statistic follows a chi-squared distribution.
}

\usage{cochran.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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
\item{statistic}{the Cochran test statistic.}
\item{parameter}{the parameter(s) of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Cochran Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}

\references{

Cochran, W.G. (1937). Problems Arising in the Analysis of a Series of Similar Experiments. Supplement to \emph{Journal of the Royal Statistical Society}, \bold{4:1}, 102-118.

}


\author{
Merve Kasikci
}



\examples{

library(onewaytests)

cochran.test(Sepal.Length ~ Species, data = iris)

out <- cochran.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
paircomp(out)

}

\keyword{functions}
