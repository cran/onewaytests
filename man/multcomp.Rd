\name{multcomp}
\alias{multcomp}
\alias{multcomp.default}
\alias{multcomp.owt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple Comparisons
}
\description{
\code{multcomp} is a generic function for multiple comparisons by adjusting p-values.
}

\usage{
\method{multcomp}{owt}(x, adjust.method = c("bonferroni", "holm", "hochberg", "hommel", "BH", 
  "BY", "fdr", "none"), alpha = 0.05, ...)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{owt} object.}
  \item{adjust.method}{Method for adjusting p values (see \code{\link[stats]{p.adjust}}). Default is set to "bonferroni".} 
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

out <- aov.test(iris$Sepal.Length, iris$Species)
multcomp(out)
multcomp(out, adjust.method = "hochberg")

out2 <- kw.test(iris$Sepal.Length, iris$Species)
multcomp(out2)
multcomp(out2, adjust.method = "hommel")

out3 <- kw.test(iris$Sepal.Length, iris$Species)
multcomp(out3)
multcomp(out3, adjust.method = "holm")


}

\keyword{functions}
