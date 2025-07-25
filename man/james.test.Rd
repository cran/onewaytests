\name{james.test}
\alias{james.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
James Second Order Test
}
\description{
\code{james.test} performs James second order test. This test is a heteroscedastic alternative to one-way ANOVA. The test statistic is formulated as a sum of squared standardized differences and compared to a critical value.
}

\usage{james.test(formula, data, alpha = 0.05, na.rm = TRUE, verbose = TRUE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}
  \item{data}{a tibble or data frame containing the variables in \code{formula}.} 
  \item{alpha}{a significance level. Defaults alpha = 0.05.}
   \item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
  \item{verbose}{a logical for printing output to R console.}

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with class "jt" containing the following components:
\item{statistic}{the James second order test statistic.}
\item{criticalValue}{the critical value of the James second order test statistic.}
\item{alpha}{the level of significance to assess the statistical difference.} 
\item{method}{the character string "James Second Order Test".}
\item{data}{a data frame containing the variables in which NA values (if exist) are removed.}
\item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs} gives the sample values and \code{rhs} the corresponding groups.}


}

\references{

Cribbie, R. A., Fiksenbaum, L., Keselman, H. J., Wilcox, R. R. (2012). Effect of Non-Normality on Test Statistics for One-Way Independent Groups Designs. \emph{British Journal of Mathematical and Statistical Psychology}, \bold{65}, 56-73.

Dag, O., Dolgun, A., Konar, N.M. (2018). onewaytests: An R Package for One-Way Tests in Independent Groups Designs. \emph{The R Journal}, \bold{10:1}, 175-199.

}


\author{
Anil Dolgun
}



\examples{

library(onewaytests)

james.test(Sepal.Length ~ Species, data = iris, alpha = 0.05)

out <- james.test(Sepal.Length ~ Species, data = iris, alpha = 0.05, verbose = FALSE)
summary(out)
paircomp(out)

}

\keyword{functions}
