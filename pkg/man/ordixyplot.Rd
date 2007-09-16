\name{ordixyplot}
\alias{ordixyplot}
\alias{ordisplom}
\alias{ordicloud}
\alias{panel.ordi}
\title{ Trellis (Lattice) Plots for Ordination }
\description{
  Functions \code{ordicloud}, \code{ordisplom} and \code{ordixyplot}
  provide an interface to plot ordination results using Trellis
  functions \code{\link[lattice]{cloud}}, \code{\link[lattice]{splom}}
  and \code{\link[lattice]{xyplot}} in package \pkg{lattice}. 
}
\usage{
ordixyplot(x, data = NULL, formula, display = "sites", choices = 1:3,
    panel = "panel.ordi", aspect = "iso", ...)
ordisplom(x, data=NULL, formula = NULL,  display = "sites", choices = 1:3,
    panel = "panel.ordi", ...)
ordicloud(x, data = NULL, formula, display = "sites", choices = 1:3, 
    panel = "panel.cloud", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ An ordination result that \code{\link{scores}} knows: any
    ordination result in \pkg{vegan} and many others. }
  \item{data}{ Optional data to amend ordination results. The ordination
    results are found from \code{x}, but you may give here data for other
    variables needed in plots. Typically these are environmental data.}
  \item{formula}{ Formula to define the plots. This is optional in
    \code{ordisplom}, but must be given in other functions. The
    ordination axes must be called by the same names as in the
    ordination results (and these names vary among methods). In
    \code{ordisplom}, special character \code{.} refers to the
    ordination result. }
  \item{display}{ The kind of scores: an argument passed to \code{\link{scores}}. }
  \item{choices}{ The axes selected: an argument passed to \code{\link{scores}}. }
  \item{panel}{ The name of the panel function. }
  \item{aspect}{The aspect of the plot (passed to the \pkg{lattice} function).}
  \item{\dots}{ Arguments passed to \pkg{lattice} functions. }
}
\details{
  The functions provide an interface to the corresponding \pkg{lattice}
  functions.  All graphical parameters are passed to the \pkg{lattice}
  function so that these graphs are extremely configurable. See
  \code{\link[lattice]{Lattice}} and \code{\link[lattice]{xyplot}},
    \code{\link[lattice]{splom}} and \code{\link[lattice]{cloud}} for
  details, usage and opportunities.

  The argument \code{x} must always be an ordination result. The scores
  are extracted with \pkg{vegan} function \code{\link{scores}} so that
  these functions work with all \pkg{vegan} ordinations and many others.

  The \code{formula} is optional in \code{ordisplom}, but must be given
  in other functions. If formula is omitted in \code{ordisplom} it
  produces a pairs plot of ordination axes and variables in
  \code{data}. If \code{formula} is given, ordination results must be
  referred to as \code{,} and other variables by their names. In other
  functions, the formula must use the names of ordination scores and names
  of \code{data}.

  The ordination scores are found from \code{x}, and \code{data} is
  optional. The \code{data} should contain other variables than
  ordination scores to be used in plots. Typically, they are
  environmental variables used, such as factors to define panels or plot
  symbols.

  The proper work is done by the panel function. Currently there is only
  a skeleton of \code{panel.ordi}. The layout can be changed by defining
  own panel functions. See \code{\link[lattice]{panel.xyplot}},
  \code{\link[lattice]{panel.splom}} and
  \code{\link[lattice]{panel.cloud}} for details and survey of
  possibilities.
}

\value{
  The function return \code{\link[lattice]{Lattice}} objects of class
  \code{"trellis"}.   
}
\author{Jari Oksanen }
\note{
  These are proof-of-the-concept functions that provide only a skeleton
  at the moment. Contributions are welcome.
}
\seealso{
  \code{\link[lattice]{Lattice}},
  \code{\link[lattice]{xyplot}},
  \code{\link[lattice]{splom}},
  \code{\link[lattice]{cloud}},
  \code{\link[lattice]{panel.splom}},
  \code{\link[lattice]{panel.cloud}}
}
\examples{
data(dune)
data(dune.env)
ord <- cca(dune)
## Pairs plots
ordisplom(ord)
ordisplom(ord, data=dune.env, choices=1:2)
ordisplom(ord, data=dune.env, form = ~ . | Management, groups=Manure)
## Scatter plot
ordixyplot(ord, data=dune.env, form = CA2 ~ CA1 | Management, groups=Manure)
## ... Slices of third axis
ordixyplot(ord, form = CA2 ~ CA1 | equal.count(CA3, 4))
## 3D Scatter plots
ordicloud(ord, form = CA2 ~ CA3*CA1, groups = Manure, data = dune.env)
ordicloud(ord, form = CA2 ~ CA3*CA1 | Management, groups = Manure,
   data = dune.env, auto.key = TRUE)
}

\keyword{ hplot }

