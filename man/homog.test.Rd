\name{homog.test}
\alias{homog.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variance Homogeneity Tests
}
\description{
\code{homog.test} performs variance homogeneity tests including Levene, Bartlett, Fligner-Killeen tests.
}

\usage{homog.test(formula, data, method = c("Levene", "Bartlett", "Fligner"), 
  alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in the formula \code{formula}} 
  \item{method}{a character string to select one of the variance homogeneity tests. "Levene": Levene's test, "Bartlett": Bartlett's test, "Fligner": Fligner-Killeen test.} 
  \item{alpha}{the level of significance to assess variance homogenity. Default is set to alpha = 0.05.}
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list containing the following components:
\item{statistic}{the corresponding test statistic.}
\item{parameter}{the parameter(s) of the approximate corresponding distribution of the test statistic. The corresponding distribution is F distribution for Levene's test, Chi-square disribution for Bartlett's test and Fligner-Killeen test.}
\item{p.value}{the p-value of the test.}

}


\author{
Osman Dag
}

\seealso{
\code{\link[car]{leveneTest}} \code{\link[stats]{bartlett.test}} \code{\link[stats]{fligner.test}} 
 
}



\examples{

library(onewaytests)

homog.test(Sepal.Length ~ Species, data = iris)
homog.test(Sepal.Length ~ Species, data = iris, method = "Bartlett")

}

\keyword{functions}
