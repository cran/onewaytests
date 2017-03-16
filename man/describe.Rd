\name{describe}
\alias{describe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Descriptive Statistics
}
\description{
\code{describe} produces basic descritive statistics including sample size, mean, standard deviation, median, minimum value, maximum value, 25th quantile, 75th quantile, skewness, kurtosis, the number of missing value.
}

\usage{describe(y, group = NULL)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{a numeric vector of data values.}
  \item{group}{a vector or factor object giving the group for the corresponding elements of y.} 
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

describe(iris$Sepal.Length, iris$Species)

}

\keyword{functions}
