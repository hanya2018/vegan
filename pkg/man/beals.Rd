\encoding{UTF-8}
\name{beals}
\alias{beals}
\alias{beals.sp}
\alias{swan}

\title{Beals Smoothing and Degree of Absence }
\description{
  Beals smoothing replaces each entry in the community data with a
  probability of target species occurring in that particular site, based
  on the joint occurrences of target species with the species that
  actually occur in the site.  Swan's (1970) degree of absence applies
  Beals smoothing to zero items so long that all zeros are replaced
  with smoothed values. 
}
\usage{
beals(x, refX = x, mode = 0, incSp = TRUE)
beals.sp(x, spIndex, refX = x, mode = 0, incSp = FALSE)
swan(x)
}
\arguments{
  \item{x}{Community data frame or matrix }
  \item{spIndex}{Index of the single species whose Beals smoothing
  values are to be computed. } 
  \item{refX}{ Community data frame or matrix to be used to compute
  joint occurrences. If \code{refX=NULL} then \code{x} is used to
  compute the joint occurrences.} 
  \item{incSp}{ This flag indicates whether the target species has to be
  included when computing the mean of the conditioned probabilities. The
  original Beals (1984) definition is equivalent to \code{incSp=TRUE},
  while the formulation of \enc{Münzbergová}{Munzbergova} and Herben is
  equal to \code{incSp=FALSE}. }  
  \item{mode }{Specifies if and how abundance values have to be
  used. \code{mode = 0} presence/absence mode. \code{mode = 1}
  abundances in \code{refX} (or \code{x}) are used to compute
  conditioned probabilities. \code{mode = 2} abundances in \code{x} are
  used to compute weighted average of conditioned
  probabilities. \code{mode = 3} abundances are used to compute both
  conditioned probabilities and the weighted average.}  
}
\details{

  Beals smoothing is the estimated probability \eqn{p_{ij}}{p[ij]} that
  species \eqn{j} occurs in site \eqn{i}.  It is defined as \eqn{p_{ij}
  = \frac{1}{S_i} \sum_k \frac{N_{jk} I_{ik}}{N_k}}{p[ij] = 1/S[i]
  Sum(k) N[jk] I[ik] / N[k]}, where \eqn{S_i}{S[i]} is the number of
  species on site \eqn{i}, \eqn{N_{jk}}{N[jk]} is the number of joint
  occurrences of species \eqn{j} and \eqn{k}, \eqn{N_k}{N[k]} is the
  number of occurences of species \eqn{k}, and \eqn{I} is the incidence
  (0 or 1) of species (this last term is usually omitted from the
  equation, but it is necessary). As \eqn{N_{jk}}{N[jk]} can be
  interpreted as a mean of conditional probability, the \code{beals}
  fucntion can be interpred as a mean of conditioned probabilities (De
  \enc{Cáceres}{Caceres} & Legendre 2008).  The currrent function is
  generalized to abundance values (De \enc{Cáceres}{Caceres} & Legendre
  2008).

  Beals smoothing was originally suggested as a method of data
  transformation to remove excessive zeros (Beals 1984, McCune
  1994).  However, it is not a suitable method for this purpose since it
  does not maintain the information on species presences:  A species may
  have a higher probability of occurrence in a site where it does not
  occur than in sites where it occurs.  Moreover, it regularizes data
  too strongly.  The method may be useful in identifying species that
  belong to the species pool (Ewald 2002) or to identify suitable
  unoccupied patches in metapopulation analysis
  (\enc{Münzbergová}{Munzbergova} & Herben 
  2004).  In this case, the function shold be called with \code{incSp
  = FALSE} for cross-validatory smoothing for species, and function
  \code{beals.sp} can be used if onl one species was studied.

  Swan (1970) suggested replacing zero values with degrees of absence of
  a species in a community data matrix. Swan expressed the method in
  terms of a similarity matrix, but it is equivalent to applying Beals
  smoothing to zero values, at each step shifting the smallest initially
  non-zero item to value one, and repeating this so many times that
  there are no zeros left in the data. This is actually very similar to
  extended dissimilarities (implemented in function
  \code{\link{stepacross}}), but very rarely used. 
}
\value{
  The function returns a transformed data matrix.
}
\references{
  
Beals, E.W. 1984. Bray-Curtis-ordination: an effective strategy for
analysis of multivariate ecological data.  \emph{Adv. Ecol. Res.} 14:
1--55.

De \enc{Cáceres}{Caceres}, M. & Legendre, P. 2008. Beals smoothing
revisited. \emph{Oecologia} 156: 657--669.
  
Ewald, J. 2002. A probabilistic approach to estimating species pools
from large compositional matrices. \emph{J. Veg. Sci.} 13: 191--198.

McCune, B. 1994. Improving community ordination with the Beals smoothing
function. \emph{Ecoscience} 1: 82--86.

\enc{Münzbergová}{Munzbergova}, Z. & Herben, T. 2004. Identification of
suitable unoccupied 
habitats in metapopulation studies using co-occurrence of species. \emph{Oikos}
105: 408--414.

Swan, J.M.A. (1970) An examination of some ordination problems by use of
simulated vegetational data. \emph{Ecology} 51, 89--102. 

}
\author{Miquel De \enc{Cáceres}{Caceres} and Jari Oksanen}

\seealso{\code{\link{decostand}} for proper standardization methods,
  \code{\link{specpool}} for an attempt to assess the size of species
  pool. }
\examples{
data(dune)
## Default
x <- beals(dune)
## Remove target species
x <- beals(dune, incSp = FALSE)
## Smoothed values against presence or absence of species
pa <- decostand(dune, "pa")
boxplot(as.vector(x) ~ unlist(pa), xlab="Presence", ylab="Beals")
## Remove the bias of tarbet species: Yields lower values.
beals(dune, mode =3, incSp = FALSE)
## Uses abundance information.
## Vector with beals smoothing values corresponding to the first species
## in dune.
beals.sp(dune, spIndex=1, incSp=TRUE) 
}
\keyword{ manip }
\keyword{ smooth }
