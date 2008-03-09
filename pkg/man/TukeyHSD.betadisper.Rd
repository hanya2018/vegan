\name{TukeyHSD.betadisper}
\alias{TukeyHSD.betadisper}
\title{Compute Tukey Honest Significant Differences}

\description{
  Create a set of confidence intervals on the differences between
  the means of the levels of the grouping factor with the specified
  family-wise probability of coverage.  The intervals are based on the
  Studentized range statistic, Tukey's 'Honest Significant
  Difference' method.
}
\usage{
\method{TukeyHSD}{betadisper}(x, which = "group", ordered = FALSE,
         conf.level = 0.95, \ldots)
}
\arguments{
  \item{x}{A fitted model object of class \code{\link{betadisper}}.}
  \item{which}{A character vector listing terms in the fitted model for
    which the intervals should be calculated. Defaults to the grouping
    factor.}
  \item{ordered}{Logical; see \code{\link{TukeyHSD}}.}
  \item{conf.level}{A numeric value between zero and one giving the
    family-wise confidence level to use.}
  \item{\ldots}{Additional arguments passed to \code{\link{TukeyHSD}}.}
}
\details{
  This is a simple wrapper to \code{\link{TukeyHSD.aov}}. The user is
  directed to read the help file for \code{\link{TukeyHSD}} before using
  this function.

  In particular, note the statement about using the function with
  unbalanced designs.
}
\author{
  Douglas Bates
}
\seealso{
  \code{\link{betadisper}}, \code{\link{permDisper}},
  \code{\link{aov}}.
}
\examples{
## continue the example from ?betadisper
example(betadisper)

mod.hsd <- TukeyHSD(mod)
plot(mod.hsd)

}
\keyword{models}
\keyword{design}