\name{decostand}
\alias{decostand}
\alias{wisconsin}

\title{Standardization Methods for Community Ecology}
\description{
The function provides some popular (and effective) standardization
methods for community ecologists.
}
\usage{
decostand(x, method, MARGIN, range.global, na.rm=FALSE)

wisconsin(x)
}

\arguments{
  \item{x}{Community data, a matrix-like object.}
  \item{method}{Standardization method. See Details for available options.}
  \item{MARGIN}{Margin, if default is not acceptable. \code{1} = rows,
    and \code{2} = columns of \code{x}.}
  \item{range.global}{Matrix from which the range is found in
    \code{method = "range"}.  This allows using same ranges across
    subsets of data.  The dimensions of \code{MARGIN} must match with
    \code{x}. }
  \item{na.rm}{Ignore missing values in row or column standardizations.}
}
\details{
  The function offers following standardization methods for community
  data:
  \itemize{
    \item \code{total}: divide by margin total (default \code{MARGIN = 1}).
    \item \code{max}: divide by margin maximum (default \code{MARGIN = 2}).
    \item \code{freq}: divide by margin maximum and multiply by the
    number of non-zero items, so that the average of non-zero entries is
    one (Oksanen 1983; default \code{MARGIN = 2}).
    \item \code{normalize}: make margin sum of squares equal to one (default
    \code{MARGIN = 1}).
    \item \code{range}: standardize values into range 0 \dots 1 (default
    \code{MARGIN = 2}).  If all values are constant, they will be
    transformed to 0.
    \item \code{standardize}: scale \code{x} to zero mean and unit variance
    (default \code{MARGIN = 2}).
    \item \code{pa}: scale \code{x} to presence/absence scale (0/1).
    \item \code{chi.square}: divide by row sums and square root of
    column sums, and adjust for square root of matrix total
    (Legendre & Gallagher 2001). When used with the Euclidean
    distance, the distances should be similar to the the
    Chi-square distance used in correspondence analysis. However, the
    results from \code{\link{cmdscale}} would still differ, since
    CA is a weighted ordination method (default \code{MARGIN =
      1}).
    \item \code{hellinger}: square root of \code{method = "total"}
    (Legendre & Gallagher 2001).
  }
  Standardization, as contrasted to transformation, means that the
  entries are transformed relative to other entries.

  All methods have a default margin. \code{MARGIN=1} means rows (sites
  in a normal data set) and \code{MARGIN=2} means columns (species in a
  normal data set).

  Command \code{wisconsin} is a shortcut to common Wisconsin double
  standardization where species (\code{MARGIN=2}) are first standardized
  by maxima (\code{max}) and then sites (\code{MARGIN=1}) by
  site totals (\code{tot}).

  Most standardization methods will give nonsense results with
  negative data entries that normally should not occur in the community
  data. If there are empty sites or species (or constant with
  \code{method =  "range"}), many standardization will change these into
  \code{NaN}.  
}
\value{
  Returns the standardized data frame, and adds an attribute
  \code{"decostand"} giving the name of applied standardization
  \code{"method"}.
}
\author{Jari Oksanen}
\note{Common transformations can be made with standard \R functions.}

\references{
  Legendre, P. & Gallagher, E.D. (2001) Ecologically meaningful
  transformations for ordination of species data. \emph{Oecologia}
  \strong{129}; 271--280.

  Oksanen, J. (1983) Ordination of boreal heath-like vegetation with
  principal component analysis, correspondence analysis and
  multidimensional scaling. \emph{Vegetatio} \strong{52}; 181--189.
  }

\examples{
data(varespec)
sptrans <- decostand(varespec, "max")
apply(sptrans, 2, max)
sptrans <- wisconsin(varespec)

# Chi-square: Similar but not identical to Correspondence Analysis.
sptrans <- decostand(varespec, "chi.square")
plot(procrustes(rda(sptrans), cca(varespec)))
}
\keyword{ multivariate}%-- one or more ...
\keyword{ manip }
