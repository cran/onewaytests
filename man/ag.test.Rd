\name{ag.test}
\alias{ag.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Function to Perform Alexander-Govern Test
}
\description{
\code{ag.test} performs Alexander-Govern test.
}

\usage{ag.test(y, group, na.rm = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a numeric vector of data values.}
  \item{group}{a vector or factor object giving the group for the corresponding elements of y.} 
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with class "htest" containing the following components:\item{statistic}{the Alexander-Govern statistic.}
\item{parameter}{the parameter(s) of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}

}

\references{

Schneider, P. J., Penfield, D. A. (1997). Alexander and Govern's Approximation: Providing an Alternative to ANOVA Under Variance Heterogeneity. \emph{The Journal of Experimental Education}, \bold{65:3}, 271-286.

}


\author{
Osman Dag, Anil Dolgun, N. Meric Konar
}



\examples{

ag.test(iris$Sepal.Width,iris$Species)

}

\keyword{functions}
