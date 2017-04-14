\name{paircomp}
\alias{paircomp}
\alias{paircomp.default}
\alias{paircomp.owt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pairwise Comparisons
}
\description{
\code{paircomp} is a generic function for pairwise comparisons by adjusting p-values.
}

\usage{
\method{paircomp}{owt}(x, adjust.method = c("bonferroni", "holm", "hochberg", "hommel", "BH", 
  "BY", "fdr", "none"), ...)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{owt} object.}
  \item{adjust.method}{Method for adjusting p values (see \code{\link[stats]{p.adjust}}). Default is set to "bonferroni".} 
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

out <- aov.test(Sepal.Length ~ Species, data = iris)
paircomp(out)
paircomp(out, adjust.method = "hochberg")

out2 <- kw.test(Sepal.Length ~ Species, data = iris)
paircomp(out2)
paircomp(out2, adjust.method = "hommel")

out3 <- kw.test(Sepal.Length ~ Species, data = iris)
paircomp(out3)
paircomp(out3, adjust.method = "holm")


}

\keyword{functions}
