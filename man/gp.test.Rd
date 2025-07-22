\name{gp.test}
\alias{gp.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test for Equal Means in a One-Way Layout under Unequal Variances
}
\description{
\code{gp.test} tests whether two or more samples from normal distributions have the same means when the variances are not necessarily equal.
}

\usage{gp.test(formula, data, method = c("gtb","gtf"), alpha = 0.05, 
  na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{method}{a character string to select the method. "gtb": Generalized Test Equivalent to Parametric Bootstrap Test (size close to intended), "gtf": Generalized Test Equivalent to Fiducial Test (size assured).} 
  \item{alpha}{the level of significance to assess the statistical difference. Default is
set to alpha = 0.05.} 
  \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}


}

\note{
The methods underlying Generalized Tests are summarized in Weerahandi and Krishnamoorthy (2019), which shows that both the Fiducial and the Parametric Bootstrap tests are generalized tests based on an exact probability statement on alternative test variables. Greater details of them can be found in
Krishnamoorthy et al. (2007) and Li et al. (2011). For greater details about Generalized Inference, the reader is referred to Weerahandi (2004), which can be freely read at \href{http://www.Weerahandi.org}{Generalized Inference}.

For additional information about the methods and the code, the reader can contact the authors of this code, \href{mailto:Weerahandi@X-Techniques.com}{Sam Weerahandi} or \href{mailto:malwane.ananda@unlv.edu}{Malwane Ananda}.
}


\value{
A list with class "owt" containing the following components:
\item{p.value}{the p-value of the corresponding test.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the selected method used in generalized test.}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Daniel, W.W., Cross, C.L. (2013). \emph{Biostatistics: A Foundation for Analysis in the Health Sciences. (10th ed.)}. John Wiley and Sons, Inc.

Krishnamoorthy, K., Lu, F., Mathew, T. (2007). A parametric bootstrap approach for ANOVA with unequal variances: fixed and random models. \emph{Computational Statistics and Data Analysis}, \bold{51:12}, 5731-5742.

Li, X., Wang J., Liang H. (2011). Comparison of several means: a fiducial based approach. \emph{Computational Statistics and Data Analysis}, \bold{55:5}, 1993-2002.

Weerahandi, S. (2004). \emph{Generalized Inference in Repeated Measures: Exact Methods in MANOVA and Mixed Models}, Series in Probability and Statistics. John Wiley and Sons, Inc. 

Weerahandi, S., Krishnamoorthy, K. (2019). A note reconciling ANOVA tests under unequal error variances. \emph{Communications in Statistics-Theory and Methods}, \bold{48:3}, 689-693.


}


\author{
Sam Weerahandi, Malwane Ananda
}



\examples{

library(onewaytests)
\donttest{
gp.test(Sepal.Length ~ Species, data = iris, method = "gtb")
out <- gp.test(Sepal.Length ~ Species, data = iris, method = "gtb", verbose = FALSE)
summary(out)
paircomp(out)

gp.test(Sepal.Length ~ Species, data = iris, method = "gtf")
out <- gp.test(Sepal.Length ~ Species, data = iris, method = "gtf", verbose = FALSE)
summary(out)
paircomp(out)
}


}

\keyword{functions}
