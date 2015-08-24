\name{james.test}
\alias{james.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Function to Perform James's Second-Order Test
}
\description{
\code{james.test} performs James's second-order test.
}

\usage{james.test(y, group, alpha = 0.05)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a numeric vector of data values.}
  \item{group}{a vector or factor object giving the group for the corresponding elements of y.}
  \item{alpha}{a significance level. Defaults alpha = 0.05.}
 

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Returns a list containing following elements:
\item{statistic}{the James's Second-Order Test statistic.}
\item{criticalvalue}{the critical value of the James's Second-Order Test statistic.}
\item{result}{the result of the test.}

}

\references{

Cribbie, R. A., Fiksenbaum, L., Keselman, H. J., Wilcox, R. R. (2012). Effect of Non-Normality on Test Statistics for One-Way Independent Groups Designs. \emph{British Journal of Mathematical and Statistical Psychology}, \bold{65}, 56-73.

}


\author{
Anil Dolgun, Osman Dag, N. Meric Konar
}



\examples{

james.test(iris$Sepal.Width,iris$Species,alpha=0.05)

y=rnorm(10,5,2)
group=c(rep(1, times=3),rep("two", times=3), rep(8, times=4))
james.test(y, group, alpha=0.10)

}

\keyword{functions}
