\name{bf.test}
\alias{bf.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Function to Perform Brown-Forsythe Test
}
\description{
\code{bf.test} performs Brown-Forsythe test.
}

\usage{bf.test(y, group)}

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
\item{statistic}{the Brown-Forsythe test statistic.}
\item{df1}{the first degrees of freedom of the approximate F distribution of the test statistic.}
\item{df2}{the second degrees of freedom of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}

}

\references{

Brown, M. B., Forsythe. A. B. (1974a). The small sample behavior of some statistics which test the equality of several means. \emph{Technometrics}, \bold{16}, 129-132.

Brown, M. B., Forsythe. A. B. (1974b). Robust tests for the equality of variances. \emph{Journal of the American Statistical Association}, \bold{69}, 364-367.

}


\author{
Osman Dag, Anil Dolgun, N. Meric Konar
}



\examples{
bf.test(iris$Sepal.Width,iris$Species)

y=rnorm(10,5,2)
group=c(rep(1, times=3),rep("two", times=3), rep(8, times=4))
bf.test(y, group)
}

\keyword{functions}
