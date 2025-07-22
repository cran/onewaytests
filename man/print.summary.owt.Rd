\name{print.summary.owt}
\alias{print.summary.owt}
\title{Print Method for Summary of One-Way Test Results}

\usage{
\method{print}{summary.owt}(x, ...)}

\arguments{
  \item{x}{An object returned by \code{summary.owt()}.}
  \item{...}{Additional arguments.}
}
\description{
Prints the formatted summary of an \code{owt} object to the console.
}

\author{
Osman Dag
}

\seealso{
\code{\link{summary.owt}}
}

\examples{
out <- onewaytests(Sepal.Length ~ Species, data = iris, method = "aov", verbose = FALSE)
summary(out)
paircomp(out)

out <- aov.test(Sepal.Length ~ Species, data = iris, verbose = FALSE)
summary(out)
}

\keyword{functions}
