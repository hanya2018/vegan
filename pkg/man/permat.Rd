\name{permat}
\alias{permatfull}
\alias{permatswap}
\alias{plot.permat}
\alias{print.permat}
\alias{summary.permat}
\alias{swapcount}
\title{Matrix Permutation Algorithms or Presence-Absence and Count Data}
\description{
Individual (for count data) or incidence (for presence-absence data) based null models can be generated for community level simulations. Options for preserving characteristics of the original matrix (sum, number of incidences, rows/columns sums) and restricted permutations (within strata based on spatial units, habitat classes or both) are discussed in the Details section. The testing of different hypothesis is separated from the null model generation, thus several tests can be applied on the same set of random matrices.
}
\usage{
permatfull(m, fixedmar = "both", reg = NULL, hab = NULL, mtype = "count", times = 100)
permatswap(m, reg = NULL, hab = NULL, mtype = "count", method = "swap", times = 100, burnin = 10000, thin = 1000)
swapcount(m, thin = 1)
\method{plot}{permat}(x, ...)
\method{print}{permat}(x, digits = 3, ...)
\method{summary}{permat}(x, digits = 2, ...)
}
\arguments{
  \item{m}{a community data matrix with plots (samples) as rows and species (taxa) as columns.}
  \item{fixedmar}{character, stating which of the row/column sums should be preserved (\code{"none", "rows", "columns", "both"}).}
  \item{reg}{numeric vector or factor with length same as \code{nrow(m)} for grouping rows within strata (regions) for restricted permutations. Unique values or levels are used.}
  \item{hab}{numeric vector or factor with length same as \code{nrow(m)} for grouping rows within strata (habitat classes) for restricted permutations. Unique values or levels are used.}
  \item{mtype}{matrix data type, either \code{"count"} for count data, or \code{"prab"} for presence-absence data.}
  \item{times}{number of permuted matrices.}
  \item{method}{character for method used for the swap algorithm (\code{"swap", "tswap", "backtrack"}) as described for function \code{\link{commsimulator}}. If \code{mtype="count"} only \code("swap") is available using the function \code{\link{swapcount}}.}
  \item{burnin}{number of null communities discarded before proper analysis in sequential \code{"swap", "tswap"} methods.}
  \item{thin}{number of discarded permuted matrices between two evaluations in sequential \code{"swap", "tswap"} methods.}
  \item{x}{object of class 'permat'}
  \item{digits}{number of digits used for rounding.}
  \item{\dots}{other arguments passed to methods.}

}
\details{
Unrestricted and restricted permutations: if both \code{reg} and \code{hab} are \code{NULL}, functions perform unrestricted permutations. If either of the two is given, it is used as is for restricted permutations. If both are given, interaction is used for restricted permutations.

Constraints on row/colum sums, matrix fill, total sum and sums within strata can be chacked by the \code{summary} method.
}
\value{
Functions \code{permatfull} and \code{permatswap} return an object of class 'permat'.
  \item{call}{the function call.}
  \item{orig}{the original data matrix used for permutations.}
  \item{perm}{a list of permuted matrices with length \code{times}.}
  \item{specs}{a list of other specifications (variable in length, depending on the function used): \code{reg}, \code{hab}, \code{burnin}, \code{thin}.}

\code{summary.permat} returns a list invisibly containing mean Bray-Curtis dissimilarities calculated pairvise among original and permuted matrices, and check results of the constraints.
}
\note{
Swap methods can be very slow for large matrices.
}
\references{
Original references are given on help pages of the functions used internally, listed in section 'See Also'.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}}
\seealso{
\code{\link{commsimulator}}, \code{\link{r2dtable}}, \code{\link{sample}}
}
\examples{
library(vegan)

## A simple artificial community data matrix.
m <- matrix(c(
   1,3,2,0,3,1,
   0,2,1,0,2,1,
   0,0,1,2,0,3,
   0,0,0,1,4,3
   ), 4, 6, byrow=TRUE)
m

## The swap algorithm for count data (1 step):
a <- swapcount(m)
a
## Identiy of swapped cells:
a != m

## Using the swap algorithm to create a 
## list of permuted matrices, where
## row/columns sums and matrix fill are preserved:
x1 <- permatswap(m, burnin = 1000, thin = 100)
summary(x1)
plot(x1)

## Unrestricted permutation retaining
## row/columns sums but not matrix fill:
x2 <- permatfull(m)
summary(x2)
plot(x2)

## Unrestricted permutation of presence-absence type
## retaining neither row/columns sums nor not matrix fill:
x3 <- permatfull(m,"none", mtype="prab")
x3$orig  ## note: original matrix is binarized!
summary(x3)

## Restricted permutation,
## check sums within strata:
x4 <- permatfull(m,reg=c(1,1,2,2))
summary(x4)
}
\keyword{multivariate}
