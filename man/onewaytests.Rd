\name{onewaytests}
\alias{onewaytests}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
One-Way Tests for Independent Groups Designs
}
\description{
\code{onewaytests} is a function covering 22 one-way tests for independent groups designs.
}

\usage{onewaytests(formula, data, method = c("aov", "af", "ag", "agp", "ap", "aw", "b2", 
  "bf", "box", "cochran", "gtb", "gtf", "james", "johansen", "kw", "mbf", "pf", 
  "ss", "wa", "welch", "welch_tw", "wgf"), N = 10^5, rate = 0.1, alpha = 0.05, 
  na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{method}{the one-way test. There are 22 tests available: one-way analysis of variance ("aov"), 
  Welch's heteroscedastic F test ("welch"), Welch's heteroscedastic F test with trimmed means and Winsorized variances ("welch_tw"), 
  Brown-Forsythe test ("bf"), Alexander-Govern test ("ag"), James second order test ("james"), 
  Kruskal-Wallis test ("kw"), Scott-Smith test ("ss"), Box F test ("bf"), Generalized tests equivalent to Parametric Bootstrap ("gtb") and 
  Fiducial ("gtf") tests, Johansen F test ("johansen"), Alvandi's F test ("af"), Alvandi's generalized p-value ("agp"), approximate F test ("af"), 
  B square test ("b2"), Cochran test ("cochran"), Weerahandi's generalized F test ("wgf"), modified Brown-Forsythe test ("mbf"), 
  adjusted Welch's heteroscedastic F test ("aw"), Welch-Aspin test ("wa"), Permutation F test ("pf"). Default is set to "aov".}
  \item{N}{the number of bootstrap samples for Weerahandi's generalized F test, Alvandi's generalized p-value, and permutation F test. Default is set to 10^5.} 
  \item{rate}{the rate of observations trimmed and winsorized from each tail of the distribution for Welch's heteroscedastic F test 
  with trimmed means and Winsorized variances. Default is set to rate = 0.1.} 
  \item{alpha}{the level of significance to assess the statistical difference. Default is
set to alpha = 0.05.} 
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{

See the corresponding one-way test function.

}

\references{

Dag, O., Dolgun, A., Konar, N.M. (2018). onewaytests: An R Package for One-Way Tests in Independent Groups Designs. \emph{The R Journal}, \bold{10:1}, 175-199.

}


\author{Merve Kasikci, Osman Dag}



\examples{

library(onewaytests)

# One-Way Analysis of Variance
onewaytests(Sepal.Length ~ Species, data = iris, method = "aov")

# Alexander-Govern test
onewaytests(Sepal.Length ~ Species, data = iris, method = "ag")

# Johansen F test
onewaytests(Sepal.Length ~ Species, data = iris, method = "johansen")

out <- onewaytests(Sepal.Length ~ Species, data = iris, method = "aov")
paircomp(out)


}

\keyword{functions}
