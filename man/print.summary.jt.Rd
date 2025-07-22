\name{print.summary.jt}
\alias{print.summary.jt}
\title{Print Method for Summary of James Second Order Test Results}

\usage{
\method{print}{summary.jt}(x, ...)}

\arguments{
  \item{x}{An object returned by \code{summary.jt()}.}
  \item{...}{Additional arguments.}
}
\description{
Prints the formatted summary of an \code{jt} object to the console.
}

\author{
Merve Kasikci, Osman Dag
}

\seealso{
\code{\link{summary.jt}}
}

\examples{
out <- james.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
}

\keyword{functions}
