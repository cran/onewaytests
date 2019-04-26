\name{mw.test}
\alias{mw.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mann-Whitney U Test
}
\description{
\code{mw.test} performs Mann-Whitney U test for two samples.
}

\usage{mw.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in the formula \code{formula}} 
  \item{alpha}{the level of significance to assess the statistical difference. Default is
set to alpha = 0.05.} 
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}

\details{
Approximation to normal distribution is used to obtain the p-value.
}


\value{
A list with containing the following components:
\item{statistic}{the Z statistic.}
\item{p.value}{the p-value of the test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}

}


\author{
Osman Dag
}


\seealso{
\code{\link[stats]{wilcox.test}} \code{\link[onewaytests]{st.test}} \code{\link[onewaytests]{wt.test}} 
 
}


\examples{


library(AID)
data(AADT)

AADT$control <- factor(AADT$control, levels = c(1,2), labels = c("control","no control")) 


library(onewaytests)
describe(aadt ~ control, data = AADT)

mw.test(aadt ~ control, data = AADT)


}

\keyword{functions}
