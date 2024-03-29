\name{describe}
\alias{describe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Descriptive Statistics
}
\description{
\code{describe} produces basic descriptive statistics including sample size, mean, standard deviation, median, minimum value, maximum value, 25th quantile, 75th quantile, skewness, kurtosis, the number of missing value.
}

\usage{describe(formula, data)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Returns a data.frame of output.

}



\author{
Osman Dag
}



\examples{

library(onewaytests)

describe(Sepal.Length ~ Species, data = iris)

}

\keyword{functions}
