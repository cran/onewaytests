\name{mood.test}
\alias{mood.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Function to Perform Mood's Median Test
}
\description{
\code{mood.test} performs Mood's median test.
}

\usage{mood.test(y, group, na.rm = TRUE)}

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
A list with class "htest" containing the following components:\item{statistic}{the Mood's Median test statistic.}
\item{parameter}{the parameter(s) of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}

}

\references{

Brown, G. W., Mood, A. M. (1951). On Median Tests for Linear Hypotheses. \emph{Proceedings of the Second Berkeley Symposium on Mathematical Statististics and Probility}, University of California Press, 159-166.

}


\author{
Anil Dolgun, Osman Dag, N. Meric Konar
}



\examples{

mood.test(iris$Sepal.Width,iris$Species)

}

\keyword{functions}
