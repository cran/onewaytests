\name{kw.test}
\alias{kw.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kruskal-Wallis Test
}
\description{
\code{kw.test} performs Kruskal-Wallis test. This test serves as a nonparametric alternative to ANOVA when the normality assumption is not met. The test statistic follows a chi-squared distribution.
}

\usage{kw.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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
\item{statistic}{the Kruskal-Wallis test statistic.}
\item{parameter}{the parameter(s) of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Kruskal-Wallis Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}

\references{

Dag, O., Dolgun, A., Konar, N.M. (2018). onewaytests: An R Package for One-Way Tests in Independent Groups Designs. \emph{The R Journal}, \bold{10:1}, 175-199.

Sheskin, D. J. (2004). \emph{Handbook of Parametric and Nonparametric Statistical Procedures}. 3rd Edition. Chapman and Hall CRC. Florida: Boca Raton. 

}


\author{
Anil Dolgun
}



\examples{

library(onewaytests)

kw.test(Sepal.Length ~ Species, data = iris)

out <- kw.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
paircomp(out)

}

\keyword{functions}
