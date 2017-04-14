\name{welch.test}
\alias{welch.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Welch's Heteroscedastic F Test and Welch's Heteroscedastic F Test with Trimmed Means and Winsorized Variances
}
\description{
\code{welch.test} performs Welch's heteroscedastic F test and Welch's heteroscedastic F test with trimmed means and Winsorized variances.
}

\usage{welch.test(formula, data, rate = 0, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a data frame containing the variables in the formula \code{formula}} 
  \item{rate}{the rate of observations trimmed and winsorized from each tail of the distribution. If rate = 0, it performs Welch's heteroscedastic F test. Otherwise, Welch's heteroscedastic F test with trimmed means and Winsorized variances is performed. Default is set to rate = 0.} 
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

\item{statistic}{the value of the test statistic with a name describing it.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "Welch's Heteroscedastic F Test" or "Welch's Heteroscedastic F Test with Trimmed Means and Winsorized Variances" depending on the choice.}
\item{rate}{the rate of observations trimmed and winsorized from each tail of the distribution.} 
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}



}

\references{

Welch, B. L.(1951). On the Comparison of Several Mean Values: An Alternative Approach. \emph{Biometrika}, \bold{38}, 330-336.

}


\author{
Osman Dag
}


\examples{

welch.test(Sepal.Length ~ Species, data = iris)
welch.test(Sepal.Length ~ Species, data = iris, rate = 0.1)

}

\keyword{functions}
