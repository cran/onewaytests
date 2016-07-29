\name{welch.test}
\alias{welch.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Function to Perform Welch's Heteroscedastic F Test and Welch's Heteroscedastic F Test with Trimmed Means and Winsorized Variances
}
\description{
\code{welch.test} performs Welch's heteroscedastic F test and Welch's heteroscedastic F test with trimmed means and Winsorized variances.
}

\usage{welch.test(y, group, rate = 0, na.rm = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a numeric vector of data values.}
  \item{group}{a vector or factor object giving the group for the corresponding elements of y.} 
  \item{rate}{the rate of observations trimmed and winsorized from each tail of the distribution. If rate = 0, it performs Welch's heteroscedastic F test. Otherwise, Welch's heteroscedastic F test with trimmed means and Winsorized variances is performed. Default is set to rate = 0.} 
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with class "htest" containing the following components:\item{statistic}{the value of the test statistic with a name describing it.}
\item{parameter}{the parameter(s) of the approximate F distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
}

\references{

Welch, B. L.(1951). On the Comparison of Several Mean Values: An Alternative Approach. \emph{Biometrika}, \bold{38}, 330-336.

}


\author{
Osman Dag, Anil Dolgun, N. Meric Konar
}


\examples{

welch.test(iris$Sepal.Length, iris$Species, rate=0.1)

y=rnorm(10,5,2)
group=c(rep(1, times=3),rep("two", times=3), rep(8, times=4))
welch.test(y, group)

}

\keyword{functions}
