\name{james.test}
\alias{james.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
James Second Order Test
}
\description{
\code{james.test} performs James second order test.
}

\usage{james.test(y, group, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a numeric vector of data values.}
  \item{group}{a vector or factor object giving the group for the corresponding elements of y.}
  \item{alpha}{a significance level. Defaults alpha = 0.05.}
   \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with class "jt" containing the following components:
\item{statistic}{the James second order test statistic.}
\item{criticalValue}{the critical value of the James second order test statistic.}
\item{method}{the character string "James Second Order Test".}
\item{y}{a numeric vector of data values in which NA values (if exist) are removed.}
\item{group}{a vector or factor object giving the group for the corresponding elements of y.} 

}

\references{

Cribbie, R. A., Fiksenbaum, L., Keselman, H. J., Wilcox, R. R. (2012). Effect of Non-Normality on Test Statistics for One-Way Independent Groups Designs. \emph{British Journal of Mathematical and Statistical Psychology}, \bold{65}, 56-73.

}


\author{
Anil Dolgun
}



\examples{

james.test(iris$Sepal.Width, iris$Species, alpha = 0.05)

}

\keyword{functions}
