\encoding{UTF-8}
\name{permat}
\alias{permatfull}
\alias{permatswap}
\alias{plot.permat}
\alias{print.permat}
\alias{summary.permat}
\alias{print.summary.permat}

\title{Matrix Permutation Algorithms for Presence-Absence and Count Data}

\description{ Individual (for count data) or incidence (for
presence-absence data) based null models can be generated for
community level simulations. Options for preserving characteristics of
the original matrix (rows/columns sums, matrix fill) and
restricted permutations (within strata based on spatial units, habitat
classes or both) are discussed in the Details section. By using these functions,
hypothesis testing can be separated from the null model generation, thus
several tests might be applied on the same set of random matrices.  }

\usage{
permatfull(m, fixedmar = "both", reg = NULL, 
  hab = NULL, mtype = "count", times = 100)
permatswap(m, reg = NULL, hab = NULL, mtype = "count", 
  method = "swap", times = 100, burnin = 10000, thin = 1000)
\method{plot}{permat}(x, ...)
\method{summary}{permat}(object, ...)
\method{print}{summary.permat}(x, digits = 2, ...)
}
\arguments{
  \item{m}{a community data matrix with plots (samples) as rows and species (taxa) as columns.}
  \item{fixedmar}{character, stating which of the row/column sums should be preserved (\code{"none", "rows", "columns", "both"}).}
  \item{reg}{numeric vector or factor with length same as \code{nrow(m)} for grouping rows within strata (regions) for restricted permutations. Unique values or levels are used.}
  \item{hab}{numeric vector or factor with length same as \code{nrow(m)} for grouping rows within strata (habitat classes) for restricted permutations. Unique values or levels are used.}
  \item{mtype}{matrix data type, either \code{"count"} for count data, or \code{"prab"} for presence-absence type incidence data.}
  \item{times}{number of permuted matrices.}
  \item{method}{character for method used for the swap algorithm (\code{"swap"}, \code{"tswap"}, \code{"backtrack"}) as described for function \code{\link{commsimulator}}. If \code{mtype="count"} only \code{"swap"} is available.}
  \item{burnin}{number of null communities discarded before proper analysis in sequential (\code{"swap", "tswap"}) methods.}
  \item{thin}{number of discarded permuted matrices between two evaluations in sequential (\code{"swap", "tswap"}) methods.}
  \item{x, object}{object of class \code{"permat"}}
  \item{digits}{number of digits used for rounding.}
  \item{\dots}{other arguments passed to methods.}
} 

\details{
The function \code{permatfull} is useful when matrix fill is allowed to vary, and matrix type is \code{count}.
The \code{fixedmar} argument is used to set constraints for permutation.
If \code{none} of the margins are fixed, cells are randomised within the matrix.
If \code{rows} or \code{columns} are fixed, cells within rows or columns are randomised, respectively.
If \code{both} margins are fixed, the \code{\link{r2dtable}} function is used that is based on
Patefield's (1981) algorithm. For presence absence data, matrix fill should be necessarily fixed, and \code{permatfull}
is a wrapper for the function \code{\link{commsimulator}}. The \code{r00, r0, c0, quasiswap}
algorithms of \code{\link{commsimulator}} are used for \code{"none", "rows", "columns", "both"} values 
of the \code{fixedmar} argument, respectively 

The function \code{permatswap} is useful when matrix fill (i.e. the
proportion of empty cells) should be kept constant.  \code{permatswap}
uses different kinds of swap algorithms, and row and columns sums are
fixed in all cases.  For presence-absence data, the \code{swap} and
\code{tswap} methods of \code{\link{commsimulator}} can be used.  For
count data, an experimental swap algorithm ('swapcount') is
implemented that results in permuted matrices with fixed marginals and
matrix fill at the same time. However, it seems that this model may
not give true random matrices, and its use should be avoided in
generating Null hypotheses. The code is provided only for methods
comparisons, and may be removed from the future versions of
\pkg{vegan}.

The 'swapcount' algorithm tries to find 2x2 submatrices (identified by 2 random row and 2 random column indices), that
can be swapped in order to leave column and row totals and fill unchanged. First, the algorithm finds the largest value
in the submatrix that can be swapped (\eqn{d}) and whether in diagonal or antidiagonal way. Submatrices that contain values larger than zero in either diagonal or antidiagonal position can be swapped. Swap means that the values in diagonal or antidiagonal positions are decreased by \eqn{d}, while remaining cells are increased by \eqn{d}. A swap is made only if fill doesn't change.

Constraints on row/colum sums, matrix fill, total sum and sums within
strata can be checked by the \code{summary} method. \code{plot} method is for
visually testing the randomness of the permuted matrices, especially for the
swap algorithms. If there are any tendency in the graph, higher \code{burnin} and 
\code{thin} values can help.

Unrestricted and restricted permutations: if both
\code{reg} and \code{hab} are \code{NULL}, functions perform
unrestricted permutations. If either of the two is given, it is used
as is for restricted permutations. If both are given, interaction is
used for restricted permutations. Each strata should contain at least 2 rows
in order to perform randomization (in case of low row numbers, swap algorithms
can be rather slow).
}

\value{
Functions \code{permatfull} and \code{permatswap} return an object of class \code{"permat"}.
  \item{call}{the function call.}
  \item{orig}{the original data matrix used for permutations.}
  \item{perm}{a list of permuted matrices with length \code{times}.}
  \item{specs}{a list of other specifications (variable in length, depending on the function used): \code{reg}, \code{hab}, \code{burnin}, \code{thin}.}

\code{summary.permat} returns a list containing mean
Bray-Curtis dissimilarities calculated pairvise among original and
permuted matrices, and check results of the constraints.  }

\references{
Original references for presence-absence swap methods are given on help
page of \code{\link{commsimulator}}.

Patefield, W. M. (1981) Algorithm AS159. An efficient method of generating r x c tables with given row and column totals. 
Applied Statistics 30, 91-97.
}

\author{\enc{P\'eter S\'olymos}{Peter Solymos}, \email{solymos@ualberta.ca}; Jari Oksanen translated the original 'swapcount' algorithm for count data into C}

\seealso{
\code{\link{commsimulator}}, \code{\link{r2dtable}}, \code{\link{sample}}
}
\examples{
## A simple artificial community data matrix.
m <- matrix(c(
   1,3,2,0,3,1,
   0,2,1,0,2,1,
   0,0,1,2,0,3,
   0,0,0,1,4,3
   ), 4, 6, byrow=TRUE)
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
## not retaining row/columns sums:
x3 <- permatfull(m, "none", mtype="prab")
x3$orig  ## note: original matrix is binarized!
summary(x3)
## Restricted permutation,
## check sums within strata:
x4 <- permatfull(m, reg=c(1,1,2,2))
summary(x4)
}

\keyword{multivariate}
\keyword{datagen}

