\name{aov.test}
\alias{aov.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Function to Perform One-Way Analysis of Variance
}
\description{
\code{aov.test} performs one-way analysis of variance.
}

\usage{aov.test(y, group, na.rm = TRUE)}

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
A list with class "htest" containing the following components:\item{statistic}{the analysis of variance test statistic.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}

}

\references{

Sheskin, D. J. (2004). \emph{Handbook of Parametric and Nonparametric Statistical Procedures}. 3rd Edition. Chapman and Hall CRC. Florida: Boca Raton. 

}


\author{
Osman Dag, Anil Dolgun, N. Meric Konar
}



\examples{

aov.test(iris$Sepal.Length,iris$Species)

}

\keyword{functions}
