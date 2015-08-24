\name{aov.test}
\alias{aov.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Function to Perform One-Way Analysis of Variance.
}
\description{
\code{aov.test} performs one-way analysis of variance.
}

\usage{aov.test(y, group)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a numeric vector of data values.}
  \item{group}{a vector or factor object giving the group for the corresponding elements of y.} 

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Returns a list containing following elements:
\item{statistic}{the analysis of variance test statistic.}
\item{df1}{the first degrees of freedom of the approximate F distribution of the test statistic.}
\item{df2}{the second degrees of freedom of the approximate F distribution of the test statistic.}
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
