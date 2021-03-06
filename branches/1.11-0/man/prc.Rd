\name{prc}
\alias{prc}
\alias{summary.prc}
\alias{print.summary.prc}
\alias{plot.prc}

\title{Principal Response Curves for Treatments with Repeated Observations }
\description{
  Principal Response Curves (PRC) are a special case of Redundancy Analysis
  (\code{\link{rda}}) for multivariate responses in
  repeated observation design. They were originally suggested for
  ecological communities. They should be easier to interpret than
  traditional constrained ordination. 
}
\usage{
prc(response, treatment, time, ...)
\method{summary}{prc}(object, axis = 1, scaling = 2, digits = 4, ...)
\method{plot}{prc}(x, species = TRUE, select, scaling = 2, axis = 1, type = "l", 
    xlab, ylab, ylim, lty = 1:5, col = 1:6, pch, legpos, cex = 0.8, 
    ...)  
}

\arguments{
  \item{response}{Multivariate response data. Typically these are
    community (species) data. If the data are counts, they probably
    should be log transformed prior to the analysis. }
  \item{treatment}{A factor for treatments. }
  \item{time}{ An unordered factor defining the observations times in
    the repeated design.}
  \item{object, x}{An \code{prc} result object.}
  \item{axis}{Axis shown (only one axis can be selected).}
  \item{scaling}{Scaling of species scores, identical to the
    \code{scaling} in \code{\link{scores.rda}}.}
  \item{digits}{Number of significant digits displayed.}
  \item{species}{Display species scores.}
  \item{select}{Vector to select displayed species. This can be a vector
    of indices or a logical vector which is \code{TRUE} for the selected
    species}
  \item{type}{Type of plot: \code{"l"} for lines, \code{"p"} for points
    or \code{"b"} for both.}
  \item{xlab, ylab}{Text to replace default axis labels.}
  \item{ylim}{Limits for the vertical axis.}
  \item{lty, col, pch}{Line type, colour and plotting characters
    (defaults supplied).}
  \item{legpos}{The position of the \code{\link{legend}}. A guess is
    made if this is not supplied, and \code{NA} will suppress legend. }
  \item{cex}{Character expansion for symbols and species labels.}
  \item{\dots}{ Other parameters passed to functions. }
}
\details{
  PRC is a special case of \code{\link{rda}} with a single factor for
  \code{treatment} and a single factor for \code{time} points in repeated
  observations. In \pkg{vegan}, the corresponding \code{\link{rda}}
  model is defined as 
  \code{rda(response ~ treatment * time + Condition(time))}. Since
  the \code{time} appears twice in the model formula, its main effects
  will be aliased, and only interaction terms are available, and will be
  used in PRC. Instead of usual multivariate ordination diagrams, PRC
  uses canonical (regression) coefficients and species scores for a
  single axis. All that the current functions do is to provide a special
  \code{summary}  and \code{plot} methods that display the
  \code{\link{rda}} results in the PRC fashion. With default contrasts
  (\code{\link{contr.treatment}}) the coefficients are 
  contrasts against the first level, and the levels must be arranged so
  that the first level is the control.

  Function \code{summary} prints the species scores and the
  coefficients. Function \code{plot} plots coefficients against
  \code{time}
  using \code{\link{matplot}}, and has similar
  defaults.  The graph (and PRC) is meaningful only if the first
  \code{treatment} level is the control, as the results are contrasts
  to the first level when unordered factors are used. The plot also
  displays species scores on the right vertical axis using function
  \code{\link{linestack}}. Typically the number of species is so high
  that not all can be displayed with the default settings, but users can
  reduce character size or padding (\code{air}) in
  \code{\link{linestack}}, or \code{select} only a subset of the
  species. A legend will be displayed unless suppressed with
  \code{legpos = NA}, and the functions tries to guess where to put the
  legend if \code{legpos} is not supplied.  
  
}
\value{
  The function is a special case of \code{\link{rda}} and returns its
  result object (see \code{\link{cca.object}}). However, a special
  \code{summary} and \code{plot} methods display returns differently
  than in \code{\link{rda}}.
}
\references{
  van den Brink, P.J. & ter Braak, C.J.F. (1999). Principal response
  curves: Analysis of time-dependent multivariate responses of
  biological community to stress. Environmental Toxicology and
  Chemistry, 18, 138--148.
}
\author{ Jari Oksanen }

\section{Warning }{
  The first level of \code{treatment} must be the control, and the
  treatment contrasts must be used (see \code{\link{contr.treatment}}),
  so that results are expressed as contrasts to the control.
  The function works with other contrast types also, but then the user
  must take care that the resulting coefficients are meaningful.
  The
  \code{time} must be an unordered factor.  
} 

\seealso{\code{\link{rda}}, \code{\link{anova.cca}}.}
\examples{
# Chlorpyrifos experiment and experimental design
data(pyrifos)
week <- gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24))
dose <- factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11))
# PRC
mod <- prc(pyrifos, dose, week)
mod            # RDA
summary(mod)   # PRC
logabu <- colSums(pyrifos)
plot(mod, select = logabu > 100)
# Permutations should be done only within one week, and we only
# are interested on the first axis
anova(mod, strata = week, first=TRUE)
}
\keyword{ multivariate }

