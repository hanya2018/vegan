\encoding{UTF-8}
\name{indpower}
\alias{indpower}

\title{Indicator Power of Species}

\description{ Indicator power calculation of Halme et al.  (2009)
  or the congruence between indicator and target species.  
}

\usage{
indpower(x, type = 0)
}
\arguments{
  \item{x}{Community data frame or matrix. }
  \item{type}{The type of
  statistic to be returned. See Details for explanation.}  
}
\details{

  Halme et al. (2009) described an index of indicator power defined as
  \eqn{IP_I = \sqrt{a \times  b}}{IP_I = sqrt(a*b)}, where \eqn{a = S / O_I} and
  \eqn{b = 1 - (O_T - S) / (N - O_I)}. 
  \eqn{N} is the number of sites,
  \eqn{S} is the number of shared occurrences of the indicator (\eqn{I})
  and the target (\eqn{T}) species. \eqn{O_I} and \eqn{O_T} are number
  of occurrences of the indicator and target species. The \code{type}
  argument in the function call enables to choose which statistic to
  return. \code{type = 0} returns \eqn{IP_I}, \code{type = 1} returns 
  \eqn{a}, \code{type = 2} returns \eqn{b}.
  Total indicator power (TIP) of an indicator species is the column mean
  (without its own value, see examples).  
  Halme et al. (2009) explain how to calculate confidence
  intervals for these statistics.
}
\value{
  A matrix with indicator species as rows and
  target species as columns (this is indicated by the first letters of the
  row/column names).
}
\references{
Halme, P., \enc{Mönkkönen}{Monkkonen}, M., Kotiaho, J. S, 
\enc{Ylisirniö}{Ylisirnio}, A-L. 2009. Quantifying the indicator power
of an indicator species. \emph{Conservation Biology} 23: 1008--1016.
}
\author{Peter Solymos}

\seealso{ \code{\link[labdsv]{duleg}} (package \pkg{labdsv}) for the
indicator species analysis of \enc{Dufrêne}{Dufrene} & Legendre. 
Function \code{\link{beals}} estimates individual cell probabilities
of species occurrences. }

\examples{
data(dune)
## IP values
ip <- indpower(dune)
## and TIP values
diag(ip) <- NA
rowMeans(ip, na.rm=TRUE)
}
\keyword{ multivariate }

