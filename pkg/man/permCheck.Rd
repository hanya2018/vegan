\name{permCheck}
\alias{permCheck}
\alias{numPerms}
\alias{print.permCheck}
\alias{print.summary.permCheck}
\alias{summary.permCheck}
\alias{getNumObs}
\alias{getNumObs.default}
\alias{getNumObs.integer}
\alias{getNumObs.numeric}
\alias{allPerms}
\alias{print.allPerms}
\alias{summary.allPerms}
\alias{print.summary.allPerms}

\title{Utility functions for permutation schemes}
\description{
  \code{permCheck} provides checking of permutation schemes for
  validity. \code{numPerms} calculates the maximum number of
  permutations possible under the current permutation
  scheme. \code{allPerms} enumerates all possible permutations for the
  given scheme. \code{getNumObs} is a utility function to return the
  number of observations for a range of R and ordination objects.
}
\usage{
permCheck(object, control = permControl())

\method{summary}{permCheck}(object, \dots)

numPerms(object, control = permControl())

allPerms(n, control = permControl(), max = 9999,
         observed = FALSE)

\method{summary}{allPerms}(object, \dots)

getNumObs(object, \dots)

\method{getNumObs}{default}(object, \dots)

\method{getNumObs}{numeric}(object, \dots)

\method{getNumObs}{integer}(object, \dots)
}

\arguments{
  \item{object}{an R object. Specifically, any object handled by
    \code{\link{scores}}, data frames, matrices, and numeric and
    integer  vectors. For \code{\link{summary.permCheck}} an object of
    class \code{"permCheck"}.}
  \item{control}{a list of control values describing properties of the
    permutation design, as returned by a call to
    \code{\link{permControl}}.}
  \item{n}{the number of observations.}
  \item{max}{the maximum number of permutations, below which complete
    enumeration will be attempted. See Details.}
  \item{observed}{logical, should the observed ordering of samples be
    returned as part of the complete enumeration? Default is
    \code{FALSE} to facilitate usage in higher level functions.}
  \item{\dots}{arguments to other methods.}
}
\details{
  \code{permCheck}, \code{allPerms} and \code{numPerms} are utility
  functions for working with the new permutation schemes available in
  \code{\link{permuted.index2}}.

  \code{permCheck} is used to check the current permutation schemes
  against the object to which it will be applied. It calculates the
  maximum number of possible permutations for the number of observations
  in \code{object} and the permutation scheme described by
  \code{control}. The returned object contains component \code{control},
  an object of class \code{"permControl"} suitably modified if
  \code{permCheck} identifies a problem.

  The main problem is requesting more permutations than possible with
  the number of observations and the permutation design. In such cases,
  \code{nperm} is reduced to equal the number of possible permutations,
  and complete enumeration of all permutations is turned on
  (\code{control$complete} is set to \code{TRUE}). 

  Alternatively, if the number of possible permutations is low, and less
  than \code{control$minperm}, it is better to enumerate all possible
  permutations, and as such complete enumeration of all permutations is
  turned  on (\code{control$complete} is set to \code{TRUE}).

  Function \code{allPerms} enumerates all possible permutations for the
  number of observations and the selected permutation scheme. It has
  \code{\link{print}} and \code{\link{summary}} methods. \code{allPerms}
  returns a matrix containing all possible permutations, possibly
  containing the observed ordering (if argument \code{observed} is
  \code{TRUE}). The rows of this matrix are the various permutations and
  the columns reflect the number of samples.

  With free permutation designs, and restricted permutation schemes with
  large numbers of observations, there are a potentially huge number of
  possible permutations of the samples. It would be inefficient, not to
  mention incredibly time consuming, to enumerate them all. Storing all
  possible permutations would also become problematic in such cases. To
  control this and guard against trying to evaluate too large a number
  of permutations, if the number of possible permutations is larger than
  \code{max}, \code{allPerms} exits with an error.
}
\value{
  For \code{permCheck} a list containing the maximum number of
  permutations possible and an object of class
  \code{"\link{permControl}"}.

  For \code{allPerms}, and object of class \code{"allPerms"}, a matrix
  whose rows are the set of all possible permutations for the supplies
  number of observations and permutation scheme selected. The matrix has
  two additional attributes \code{control} and
  \code{observed}. Attribute \code{control} contains the argument
  \code{control} (possibly updated via \code{permCheck}). Attribute 
  \code{observed} contains argument \code{observed}.

  For \code{numPerms}, the (numeric) number of possible permutations.

  For \code{getNumObs}, the (numeric) number of observations in
  \code{object}.
}
%\references{
%}
%\note{
%  Currently, complete enumeration is not implemented in
%  \code{\link{permuted.index2}}. \code{permCheck} sets complete
%  enumeration in the \code{control} component of the returned object
%  but this is not acted upon in the permutations.
%
%  \code{permCheck} does reduce the \code{nperm} and \code{maxperm}
%  components of the returned object to the maximum number of possible
%  permutations, and \code{\link{permuted.index2}} obeys these
%  directives.
%}
\author{Gavin Simpson}
\seealso{\code{\link{permuted.index2}} and \code{\link{permControl}}.}

\examples{
## use example data from ?pyrifos
example(pyrifos)

## Demonstrate the maximum number of permutations for the pyrifos data
## under a series of permutation schemes

## no restrictions - lots of perms
(check1 <- permCheck(pyrifos, control = permControl(type = "free")))
summary(check1)

## no strata but data are series with no mirroring, so 132 permutations
permCheck(pyrifos, control = permControl(type = "series",
                   mirror = FALSE))

## no strata but data are series with mirroring, so 264 permutations
permCheck(pyrifos, control = permControl(type = "series",
                   mirror = TRUE))

## unrestricted within strata
permCheck(pyrifos, control = permControl(strata = ditch,
                   type = "free"))

## time series within strata, no mirroring
permCheck(pyrifos, control = permControl(strata = ditch,
                   type = "series", mirror = FALSE))

## time series within strata, with mirroring
permCheck(pyrifos, control = permControl(strata = ditch,
                   type = "series", mirror = TRUE))

## time series within strata, no mirroring, same permutation within strata
permCheck(pyrifos, control = permControl(strata = ditch,
                   type = "series", constant = TRUE))

## time series within strata, with mirroring, same permutation within strata
permCheck(pyrifos, control = permControl(strata = ditch,
                   type = "series", mirror = TRUE, constant = TRUE))

## permute strata
permCheck(pyrifos, permControl(strata = ditch, type = "strata"))

## this should also also for arbitrary vectors
vec1 <- permCheck(1:100)
vec2 <- permCheck(1:100, permControl())
all.equal(vec1, vec2)
vec3 <- permCheck(1:100, permControl(type = "series"))
all.equal(100, vec3$n)
vec4 <- permCheck(1:100, permControl(type = "series", mirror = TRUE))
all.equal(vec4$n, 200)

## enumerate all possible permutations
fac <- gl(2,6)
ctrl <- permControl(type = "grid", mirror = FALSE, strata = fac,
                    constant = TRUE, nrow = 3, ncol = 2)
numPerms(1:12, control = ctrl)
(tmp <- allPerms(12, control = ctrl, observed = TRUE))
(tmp2 <- allPerms(12, control = ctrl))
## turn on mirroring
ctrl$mirror <- TRUE
numPerms(1:12, control = ctrl)
(tmp3 <- allPerms(12, control = ctrl, observed = TRUE))
(tmp4 <- allPerms(12, control = ctrl))
## prints out details of the permutation scheme as
## well as the matrix of permutations
summary(tmp)
summary(tmp2)
}
\keyword{ utilities }
\keyword{ design }

