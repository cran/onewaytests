\name{ag.test}
\alias{ag.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alexander-Govern Test
}
\description{
\code{ag.test} performs Alexander-Govern test.
}

\usage{ag.test(y, group, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

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
\item{statistic}{the Alexander-Govern test statistic.}
\item{parameter}{the parameter(s) of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{method}{the character string "Alexander-Govern Test".}
\item{y}{a numeric vector of data values in which NA values (if exist) are removed.}
\item{group}{a vector or factor object giving the group for the corresponding elements of y.} 


}

\references{

Schneider, P. J., Penfield, D. A. (1997). Alexander and Govern's Approximation: Providing an Alternative to ANOVA Under Variance Heterogeneity. \emph{The Journal of Experimental Education}, \bold{65:3}, 271-286.

}


\author{
Osman Dag
}



\examples{

ag.test(iris$Sepal.Width, iris$Species)

}

\keyword{functions}
