\name{summary.owt}
\alias{summary.owt}
\title{Summary Method for One-Way Test Results}


\usage{
\method{summary}{owt}(object, detail = TRUE, ...)}

\arguments{
  \item{object}{An object of class \code{owt}, typically returned by one-way tests.}
  \item{detail}{a logical for printing detail of the one-way tests.}
  \item{...}{Additional arguments.}
}

\value{
  Prints a summary to the console.
}

\description{
Provides a concise summary of the results from an one-way test in the package.
}

\details{
This method is specifically designed for objects of class \code{owt}. It prints test method, dependent variable, grouping variable, test statistic, degrees of freedom, p-value, and any relevant notes.
}

\author{
Merve Kasikci, Osman Dag
}

\examples{
out <- onewaytests(Sepal.Length ~ Species, data = iris, method = "aov", verbose = FALSE)
summary(out)
paircomp(out)

out <- aov.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
}

\keyword{functions}
