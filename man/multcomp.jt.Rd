\name{multcomp.jt}
\alias{multcomp.jt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple Comparisons for James Second Order Test
}
\description{
\code{multcomp.jt} performs multiple comparisons by adjusting the level of significance for James second order test.
}

\usage{
\method{multcomp}{jt}(x, adjust.method = c("bonferroni", "none"), alpha = 0.05, ...)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{jt} object.}
  \item{adjust.method}{Method for adjusting the significance level. "bonferroni": Bonferroni correction, "none": No correction.} 
  \item{alpha}{the level of significance to assess the statistical difference. Default is set to alpha = 0.05.} 
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

out <- james.test(iris$Sepal.Length, iris$Species, alpha = 0.05)
multcomp(out, adjust.method = "bonferroni")

}

\keyword{functions}
