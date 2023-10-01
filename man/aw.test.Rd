\name{aw.test}
\alias{aw.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adjusted Welch's Heteroscedastic F Test
}
\description{
\code{aw.test} performs adjusted Welch's heteroscedastic F test.
}

\usage{aw.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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

\item{statistic}{the adjusted Welch's heteroscedastic F test statistic.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Adjusted Welch's Heteroscedastic F Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}


\references{
Hartung, J., Argac, D., Makambi, K.H. (2002). Small Sample Properties of Tests on Homogeneity in One-Way ANOVA and Meta-Analysis. \emph{Statistial Papers}, \bold{43:2}, 197-235.
}


\author{
Osman Dag
}



\examples{

library(onewaytests)

aw.test(Sepal.Length ~ Species, data = iris)

out <- aw.test(Sepal.Length ~ Species, data = iris)
paircomp(out)


}

\keyword{functions}
