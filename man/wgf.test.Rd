\name{wgf.test}
\alias{wgf.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weerahandi's Generalized F Test
}
\description{
\code{wgf.test} performs Weerahandi's generalized F test. This test provides a robust procedure for independent groups design by replacing the usual means and variances with trimmed means and Winsorized variances. The p-value of this test is obtained using Monte Carlo simulation.
}

\usage{wgf.test(formula, data, N = 10^5, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}


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

\note{
The user can contact the author of this code, \href{mailto:Weerahandi@X-Techniques.com}{Sam Weerahandi}, for additional information about the method and the code.
}




\value{
A list with class "owt" containing the following components:
\item{p.value}{the p-value of Weerahandi's generalized F test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Weerahandi's Generalized F Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
\item{N}{the number of bootstrap samples.} 

}

\references{

Weerahandi, S. (1995). ANOVA under Unequal Error Variances. \emph{Biometrics}, 589-599.

}


\author{
Sam Weerahandi
}

\examples{

library(onewaytests)

\donttest{
wgf.test(Sepal.Length ~ Species, data = iris)

out <- wgf.test(Sepal.Length ~ Species, data = iris)
summary(out)
paircomp(out)
}

}


\keyword{functions}
