\name{pf.test}
\alias{pf.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Permutation F Test
}
\description{
\code{pf.test} performs Permutation F test.
}

\usage{pf.test(formula, data, N = 10^5, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}


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
\item{p.value}{the Permutation F test p-value.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Permutation F Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
\item{N}{the number of bootstrap samples.} 

}

\references{

Berry, K.J., Mielke Jr, P.W., Mielke, H.W. (2002). The Fisher-Pitman Permutation Test: an Attractive Alternative to the F Test. \emph{Psychological Reports}, \bold{90:2}, 495-502.

}


\author{
Osman Dag
}

\examples{

library(onewaytests)

\donttest{
pf.test(Sepal.Length ~ Species, data = iris)

out <- pf.test(Sepal.Length ~ Species, data = iris)
paircomp(out)
}

}


\keyword{functions}
