\name{gp.test}
\alias{gp.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test for Equal Means in a One-Way Layout under Unequal Variances
}
\description{
\code{gp.test} tests whether two or more samples from normal distributions have the same means when the variances are not necessarily equal.
}

\usage{gp.test(formula, data, method = c("GT_Bootstrap","GT_Fiducial"), alpha = 0.05, 
  na.rm = TRUE, 
  verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in the formula \code{formula}} 
  \item{method}{a character string to select the method. "GT_Bootstrap": Generalized Test Equivalent to Parametric Bootstrap Test (size close to intended), "GT_Fiducial": Generalized Test Equivalent to Fiducial Test (size assured).} 
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

##Both examples given below are from the book written by Daniel and Cross (2013). 
##They are One-way ANOVA examples, where it is not reasonable to assume equal variances.

###Example 1

library(onewaytests)

x <- factor(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4))
y <- c(71.8,66.1,67.6,66.4,42.8,53.2,56.1,56.5, 
       72.5,62.9,58.9,69.3,47.1,86.6,56)

Example1 <- data.frame(y, x)

describe(y ~ x, data = Example1)

\dontrun{
out <- gp.test(y ~ x, data = Example1, alpha = 0.10)
paircomp(out)

gp.test(y ~ x, data = Example1, method = "GT_Fiducial")
}

###Example 2

library(onewaytests)

x <- factor(c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3))
y <- c(92,93,74,80.5,76,71,75.5,88.5,93,80.5,83,87,79,
       78,100,76.5,68,81.5,75,76.5,70.5,69,73.8,74,80)

Example2 <- data.frame(y, x)

describe(y ~ x, data = Example2)

\dontrun{
out <- gp.test(y ~ x, data = Example2, method = "GT_Fiducial", alpha = 0.10)
paircomp(out)

out <- gp.test(y ~ x, data = Example2, 
method = "GT_Bootstrap", alpha = 0.10)
paircomp(out)
}
}

\keyword{functions}
