\name{agp.test}
\alias{agp.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alvandi's Generalized P-Value
}
\description{
\code{agp.test} performs Alvandi's generalized p-value.
}

\usage{agp.test(formula, data, N = 10^5, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{N}{the number of bootstrap samples. Default is set to 10^5.} 
  \item{alpha}{the level of significance to assess the statistical difference. Default is
set to alpha = 0.05.} 
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}



\value{
A list with class "owt" containing the following components:
\item{p.value}{the Alvandi's generalized p-value.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Alvandi's Generalized P-Value".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
\item{N}{the number of bootstrap samples.} 

}

\references{

Sadooghi-Alvandi, S.M., Jafari, A.A., Mardani-Fard, H.A. (2012). One-Way ANOVA with Unequal Variances. \emph{Communications in Statistics-Theory and Methods}, \bold{41:22}, 4200-4221.

}


\author{
Merve Kasikci
}

\examples{

library(onewaytests)

\donttest{
agp.test(Sepal.Length ~ Species, data = iris)

out <- agp.test(Sepal.Length ~ Species, data = iris)
paircomp(out)
}

}


\keyword{functions}
