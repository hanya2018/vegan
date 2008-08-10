\name{treedive}
\alias{treedive}
\alias{treeheight}

\title{ Functional Diversity estimated from Dendrogram Heights}
\description{
Functional diversity is defined as the total heights of connecting
segments for a dendrogram of species properties for all species
occurring in a site (Petchey and Gaston). 
}
\usage{
treedive(comm, tree)
treeheight(tree)
}

\arguments{
  \item{comm}{Community data frame or matrix.}
  \item{tree}{A dendrogram which for \code{treedive} must be for species.}
}
\details{
\dots To come \dots
}
\value{
Diversity values rows of \code{comm} (a vector).
}
\references{Petchey and Gaston}
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

