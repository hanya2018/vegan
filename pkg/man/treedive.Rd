\name{treedive}
\alias{treedive}
\alias{treeheight}

\title{ Functional Diversity estimated from a Species Dendrogram}
\description{
  Functional diversity is defined as the total branch length in a trait
  dendrogram connecting all species, but excluding the unnecessary root
  segments of the tree (Petchey and Gaston 2006).
}

\usage{
treedive(comm, tree)
treeheight(tree)
}

\arguments{
  \item{comm}{Community data frame or matrix.}
  \item{tree}{A dendrogram which for \code{treedive} must be for species
    (columns).}
}
\details{
  Function \code{treeheight} finds the sum of lengths of connecting
  segments in a dendrogram produced by \code{\link{hclust}}, or other
  dendrogram that can be coerced to a correct type using
  \code{\link{as.hclust}}. When applied to a clustering of species
  traits, this is a measure of functional diversity (Petchey and Gaston
  2002, 2006).

  Function \code{treedive} finds the \code{treeheight} for each site
  (row) of a community matrix. The function uses a subset of dendrogram
  for those species that occur in each site, and excludes the tree root
  if that is not needed to connect the species (Petchey and Gaston
  2006). The subset of the dendrogram is found by first calculating
  \code{\link{cophenetic}} distances from the input dendrogram, then
  reconstructing the dendrogram for the subset of the cophenetic
  distance matrix for species occuring in each site.

  The functions need a dendrogram of species traits as an input. If
  species traits contain \code{\link{factor}} or \code{\link{ordered}}
  factor variables, it is recommended to use Gower distances for mixed
  data (function \code{\link[cluster]{daisy}} in package \pkg{cluster}),
  and usually the recommended clustering method is UPGMA (\code{method =
  "average"} in function \code{\link{hclust}}) (Podani and Schmera
  2006).
}

\value{
  A vector of diversity values or a single tree height.
}
\references{
  Petchey, O.L. and Gaston, K.J. 2002. Funcional diversity (FD), species
  richness and community composition. \emph{Ecology Letters} 5,
  402--411.
  
  Petchey, O.L. and Gaston, K.J. 2006. Functional diversity: back to
  basics and looking forward. \emph{Ecology Letters} 9, 741--758.

  Podani J. and Schmera, D. 2006. On dendrogram-based methods of
  functional diversity. \emph{Oikos} 115, 179--185.
}

\author{Jari Oksanen}

\seealso{ \code{\link{taxondive}} is something very similar from
another world. }
\examples{
## There is no data set on species properties yet, and therefore
## the example uses taxonomy 
data(dune)
data(dune.taxon)
d <- taxa2dist(dune.taxon)
cl <- hclust(d, "aver")
treedive(dune, cl)
}

\keyword{ univar }

