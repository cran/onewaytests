\name{paircomp.jt}
\alias{paircomp.jt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pairwise Comparisons for James Second Order Test
}
\description{
\code{paircomp.jt} performs multiple comparisons by adjusting the level of significance for James second order test.
}

\usage{
\method{paircomp}{jt}(x, adjust.method = c("bonferroni", "none"), verbose = TRUE, ...)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{jt} object.}
  \item{adjust.method}{Method for adjusting the significance level. "bonferroni": Bonferroni correction, "none": No correction.} 
  \item{verbose}{a logical for printing output to R console.}
  \item{...}{Additional arguments affecting multiple comparisons of groups in one-way independent designs.}

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
Returns a data.frame of output.
}



\author{
Osman Dag}



\examples{

library(onewaytests)

out <- james.test(Sepal.Length ~ Species, data = iris, alpha = 0.05)
summary(out)
paircomp(out, adjust.method = "bonferroni")

}

\keyword{functions}
