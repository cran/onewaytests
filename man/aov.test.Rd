\name{aov.test}
\alias{aov.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
One-Way Analysis of Variance
}
\description{
\code{aov.test} performs one-way analysis of variance.
}

\usage{aov.test(y, group, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a numeric vector of data values.}
  \item{group}{a vector or factor object giving the group for the corresponding elements of y.} 
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

\item{statistic}{the analysis of variance test statistic.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{method}{the character string "One-Way Analysis of Variance".}
\item{y}{a numeric vector of data values in which NA values (if exist) are removed.}
\item{group}{a vector or factor object giving the group for the corresponding elements of y.} 

}

\references{

Sheskin, D. J. (2004). \emph{Handbook of Parametric and Nonparametric Statistical Procedures}. 3rd Edition. Chapman and Hall CRC. Florida: Boca Raton. 

}


\author{
Osman Dag}



\examples{

aov.test(iris$Sepal.Length, iris$Species)

}

\keyword{functions}
