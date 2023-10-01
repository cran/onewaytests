\name{bf.test}
\alias{bf.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Brown-Forsythe Test
}
\description{
\code{bf.test} performs Brown-Forsythe test.
}

\usage{bf.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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
A list with class "owt" containing the following components:\item{statistic}{the Brown-Forsythe test statistic.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Brown-Forsythe Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Brown, M. B., Forsythe. A. B. (1974a). The small sample behavior of some statistics which test the equality of several means. \emph{Technometrics}, \bold{16}, 129-132.

Dag, O., Dolgun, A., Konar, N.M. (2018). onewaytests: An R Package for One-Way Tests in Independent Groups Designs. \emph{The R Journal}, \bold{10:1}, 175-199.

}


\author{
Osman Dag
}



\examples{

library(onewaytests)

bf.test(Sepal.Length ~ Species, data = iris)

out <- bf.test(Sepal.Length ~ Species, data = iris)
paircomp(out)


}

\keyword{functions}
