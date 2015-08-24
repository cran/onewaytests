\name{kw.test}
\alias{kw.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Function to Perform Kruskal-Wallis Rank Sum Test
}
\description{
\code{kw.test} performs Kruskal-Wallis rank sum test.
}

\usage{kw.test(y, group)}

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
\item{statistic}{the Kruskal-Wallis rank sum statistic.}
\item{df}{the degrees of freedom of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}

}

\references{

Sheskin, D. J. (2004). \emph{Handbook of Parametric and Nonparametric Statistical Procedures}. 3rd Edition. Chapman and Hall CRC. Florida: Boca Raton. 

}


\author{
Anil Dolgun, Osman Dag, N. Meric Konar
}



\examples{

kw.test(iris$Sepal.Length,iris$Species)

}

\keyword{functions}