\name{ordisurf}
\alias{ordisurf}

\title{ Fit and Plot Smooth Surfaces of Variables on Ordination. }
\description{
  Function \code{ordisurf} fits a smooth surface for given variable and
  plots the result on ordination diagram.
}
\usage{
ordisurf(x, y, choices=c(1, 2), knots=10, family="gaussian", col="red",
     thinplate = TRUE, add = FALSE, display = "sites",
     w = weights(x), main, nlevels = 10, levels, labcex = 0.6,  ...)
}

\arguments{
  \item{x}{Ordination configuration, either a matrix or a result known
    by \code{\link{scores}}. }
  \item{y}{ Variable to be plotted. }
  \item{choices}{Ordination axes. }
  \item{knots}{Number of initial knots in \code{\link[mgcv]{gam}} (one
    more than degrees of freedom). }
  \item{family}{ Error distribution in  \code{\link[mgcv]{gam}}. }
  \item{col}{ Colour of contours. }
  \item{thinplate}{Use thinplate splines in \code{\link[mgcv]{gam}}.}
  \item{add}{Add contours on an existing diagram or draw a new plot. }
  \item{display}{Type of scores known by \code{\link{scores}}: typically
    "sites" for ordinary site scores or "lc" for linear combination scores.}
  \item{w}{Prior weights on the data. Concerns mainly \code{\link{cca}}
    and \code{\link{decorana}} results which have nonconstant weights.}
  \item{main}{The main title for the plot, or as default the name of
    plotted variable in a new plot.}
  \item{nlevels, levels}{Either a vector of \code{levels} for which contours
    are drawn, or suggested number of contours in
    \code{nlevels} if \code{levels} are not supplied.}
  \item{labcex}{Label size in contours.  Setting this zero will suppress
    labels.}
  \item{\dots}{ Other graphical parameters. }
}
\details{
  Function \code{ordisurf} fits a smooth surface using thinplate splines
  in \code{\link[mgcv]{gam}}, and uses \code{\link[mgcv]{predict.gam}}
  to find fitted values in a regular grid.
  Function plots the fitted contours with convex hull of data poitns 
  either over an existing ordination diagram or draws a new plot
  The function uses
  \code{\link{scores}} to extract ordination scores, and \code{x} can be
  any result object known by that function.

  User can supply a vector of prior weights \code{w}. If the ordination
  object has weights, these will be used. In practise this means that
  the row totals are used as weights with
  \code{\link{cca}} or
  \code{\link{decorana}} results. If you do not like this, but want to give
  equal weights to all sites, you should set \code{w = NULL}. The
  behaviour is consistent with \code{\link{envfit}}. For complete
  accordance with constrained \code{\link{cca}}, you should set
  \code{display = "lc"} (and possibly \code{scaling = 2}).
}
\value{
  Function is usually called for its side effect of drawing the contour
  plot, but it returns the result object of \code{\link[mgcv]{gam}}.
}
\author{ Dave Roberts and Jari Oksanen }
\note{   
  The default is to use thinplate splines.  These make sense in
  ordination as they have equal smoothing in all directions and are
  rotation invariant. 
}

\seealso{ For basic routines \code{\link[mgcv]{gam}},
  and \code{\link{scores}}. Function 
  \code{\link{envfit}} provides a more traditional and compact
  alternative. } 

\examples{
data(varespec)
data(varechem)
library(MASS)
vare.dist <- vegdist(varespec)
vare.mds <- isoMDS(vare.dist)
with(varechem, ordisurf(vare.mds, Baresoil))
## Cover of Cladina arbuscula
with(varespec, ordisurf(vare.mds, Cla.arb, family=quasipoisson)) 
}
\keyword{ multivariate }
\keyword{ aplot }
