\name{af.test}
\alias{af.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alvandi's F Test 
}
\description{
\code{af.test} performs Alvandi's F test. This test assumes that the data within each group are normally distributed and offers a robust alternative to one-way ANOVA when heteroscedasticity is present. The test statistic follows an F distribution.
}

\usage{af.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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

\item{statistic}{the Alvandi's F test statistic.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Alvandi's F Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Sadooghi-Alvandi, S.M., Jafari, A.A., Mardani-Fard, H.A. (2012). One-Way ANOVA with Unequal Variances. \emph{Communications in Statistics-Theory and Methods}, \bold{41:22}, 4200-4221.

}


\author{
Merve Kasikci
}



\examples{

library(onewaytests)

af.test(Sepal.Length ~ Species, data = iris)

out <- af.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
paircomp(out)


}

\keyword{functions}
