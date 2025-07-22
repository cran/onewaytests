\name{summary.jt}
\alias{summary.jt}
\title{Summary Method for James Second Order Test Results}


\usage{
\method{summary}{jt}(object, detail = TRUE, ...)}

\arguments{
  \item{object}{An object of class \code{jt}, typically returned by \code{james.test}.}
  \item{detail}{a logical for printing detail of the \code{james.test}.}
  \item{...}{Additional arguments.}
}

\value{
  Prints a summary to the console.
}

\description{
Provides a concise summary of the results from an one-way test in the package.
}

\details{
This method is specifically designed for objects of class \code{jt}. It prints test method, dependent variable, grouping variable, test statistic, critical value, and any relevant notes.
}

\author{
Merve Kasikci, Osman Dag
}

\examples{
out <- james.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
}

\keyword{functions}
