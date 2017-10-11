\name{ag.test}
\alias{ag.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alexander-Govern Test
}
\description{
\code{ag.test} performs Alexander-Govern test.
}

\usage{ag.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a data frame containing the variables in the formula \code{formula}} 
  \item{alpha}{the level of significance to assess the statistical difference. Default is
set to alpha = 0.05.} 
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}


}

\note{
An R implementation of Alexander-Govern test has been available since 2007 (written by Sven Hartenstein). The website link is \href{https://eval-serv2.metpsy.uni-jena.de/wiki-metheval-hp/index.php/R_FUN_Alexander-Govern}{here}.
}


\value{
A list with class "owt" containing the following components:
\item{statistic}{the Alexander-Govern test statistic.}
\item{parameter}{the parameter(s) of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Alexander-Govern Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Schneider, P. J., Penfield, D. A. (1997). Alexander and Govern's Approximation: Providing an Alternative to ANOVA Under Variance Heterogeneity. \emph{The Journal of Experimental Education}, \bold{65:3}, 271-286.

}


\author{
Osman Dag
}



\examples{

library(onewaytests)

ag.test(Sepal.Length ~ Species, data = iris)

}

\keyword{functions}
