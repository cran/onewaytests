\name{wt.test}
\alias{wt.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Welch's t-Test
}
\description{
\code{wt.test} performs Welch's t-test for two samples. This test is an alternative to Student's t-test when variances are homogeneous. The test statistic follows a t-distribution.
}

\usage{wt.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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
\item{statistic}{the Welch's t-test statistic.}
\item{parameter}{the parameter(s) of the approximate t distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}


\author{
Osman Dag
}


\seealso{
\code{\link[stats]{t.test}} \code{\link[onewaytests]{st.test}} 
 
}


\examples{

library(AID)
data(AADT)

library(onewaytests)
describe(aadt ~ control, data = AADT)

wt.test(aadt ~ control, data = AADT)

out <- wt.test(aadt ~ control, data = AADT)
summary(out)

}

\keyword{functions}
