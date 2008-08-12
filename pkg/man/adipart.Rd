\name{adipart}
\alias{adipart}
\alias{plot.adipart}
\alias{print.adipart}
\alias{summary.adipart}
\alias{print.summary.adipart}
\title{Additive Diversity Partitioning}
\description{
In additive diversity partitioning, mean values of alpha diversity at lower levels of a sampling hierarchy are compared to the total number of species in the entire data set (gamma diversity) in the form: gamma = mean(alpha) + beta. Thus beta = gamma - mean(alpha).
}
\usage{
adipart(matr, strata, hclass = NULL, method="trad", index=c("richness", "shannon", "simpson"),
scales=seq(0, 2, 0.2), weights = "unif", test = TRUE, permtype = "full", times = 100, crit = 0.05, 
burnin = 10000, results = FALSE, ...)
\method{print}{adipart}(x, ...)
\method{summary}{adipart}(object, digits = 3, ...)
\method{print}{summary.adipart}(x, ...)
\method{plot}{adipart}(x, rel.yax = NULL, ymax = NULL, p.legend = "bottomright", ...)
}
\arguments{
  \item{matr}{a community data matrix with samples as rows and species as column, or a permutation object of class 'permat' (see Details).}
  \item{strata}{a matrix or data frame containing levels of sampling hierarchy as columns, columns from left to right will be treated as nested.}
  \item{hclass}{vector for coding habitat classes.}
  \item{method}{character, either \code{"trad"} for traditional diversity indices (see \code{index} argument for specifications), or \code{"tsallis"} for Tsallis diversity (see \code{scales} argument for specifications).}
  \item{index}{character, one of (if \code{habitat} is not \code{NULL}) or combination of (if \code{habitat = NULL}) \code{c("richness", "shannon", "simpson")}.}
  \item{scales}{vector for scales of the Tsallis diversity (see \code{\link{tsallis}} for details).}
  \item{weights}{character, \code{"unif"} for uniform weights, \code{"prop"} for weighting proportional to sample abundances.}
  \item{test}{logical, whether a permutation test should be applied.}
  \item{permtype}{character, \code{"full"} for permutation of community matrix via \code{\link{permatfull}}, \code{"swap"} for permutation via \code{\link{permatswap}}. Only used if \code{test = TRUE} and \code{matr} is not of class 'permat'.}
  \item{times}{number of permutation to use if \code{matr} is not of class 'permat'.}
  \item{crit}{two sided critical level for calculating confidence limits of the expected values. Only used if \code{test = TRUE}.}
  \item{burnin}{number of burnin steps when \code{permtype = "swap"} (see \code{\link{permatswap}}).}
  \item{results}{logical, whether null model results (individual alpha and beta diversity values) should be returned, only available for "oneway" design (\code{habitat = NULL}) if \code{test = TRUE}.}
  \item{x, object}{an object of class 'adp'.}
  \item{digits}{number of significant digits to use in the output.}
  \item{rel.yax}{logical or \code{NULL}, \code{TRUE} for relative scaling of the y axis. The default (\code{NULL}) sets the scaling according to specific design ("oneway" or "twoway") criteria.}
  \item{ymax}{maximum of the vertical axis of the plot. Useful when \code{rel.yax = FALSE} and more than one plot should have same scales. \code{ymax} is ignored if it is lower than highest value to be plotted.}
  \item{p.legend}{position of the legend box for P values, can be character of two coordinates, see \code{\link{legend}} for details. Use extreme large values to move the box out of the plotting region.}
  \item{\dots}{other arguments, e.g. arguments for \code{\link{permatfull}}, \code{\link{permatswap}}, \code{\link{par}} or \code{\link{print}}.}
}
\details{
Soon to be updated.

Details here on one- and twoway designs, indices & Tsallis.

Also randomisation (permat object, or for large problems, better to do it within adipart function call).
}
\note{
Please ensure thet \code{strata} and \code{habitat} are meaningfully compiled (i.e. there are no missing states of the combinations), because at the moment there are no checks for that in the code.
}
\value{
An object of class 'adipart':
  \item{input}{input objects (\code{m = matr}, \code{f = strata}, \code{h = habitat}) and the function call (\code{call}).}
  \item{obs}{observed diversity components (alpha, beta, and SE for alpha).}
  \item{exp}{expected diversity components, both elements with items \code{p.value}, \code{mean}, lower and upper confidence limits (\code{cl1}, \code{cl2}) and starndardized effect size (\code{ses}, (observed - mean(expected)) / sd(expexted)):
    \item{alpha}{expected alpha components,}
    \item{beta}{expected beta components.}
    }
  \item{res}{null model distribution for the tested alpha and beta diversity values (\code{NULL} if \code{results = FALSE}):
    \item{alpha}{null model distribution for alpha diversity, rows are permutations, columns are elements according to the \code{obs$alpha} matrix without the last row for gamma diversity,}
    \item{beta}{null model distribution for beta diversity, rows are permutations, columns are elements according to the \code{obs$beta} matrix.}
    }
}
\references{
Lande, R. 1996. Statistics and partitioning of species diversity, and similarity among multiple communities. Oikos, 76, 5-13.

Crist, T.O., Veech, J.A., Gering, J.C. and Summerville, K.S. 2003. Partitioning species diversity across landscapes and regions: a hierarchical analysis of $\alpha$, $\beta$, and $\gamma$-diversity. Am. Nat., 162, 734-743.

Wagner, H. H., Wildi, O. and Ewald, K.C. 2000. Additive partitioning of plant species diversity in an agricultural mosaic landscape. Landscape Ecology, 15, 219-227.
}
\author{Peter Solymos, \email{Solymos.Peter@aotk.szie.hu}}
\seealso{See \code{\link{permatfull}} and \code{\link{permatswap}} for permutation settings, and \code{\link{tsallis}} for Tsallis entropy.}
\examples{
data(mite)
data(mite.xy)
data(mite.env)
## Function to get equal area partitions of the mite data
cutter <- function (x, cut = seq(0, 10, by = 2.5)) {
    out <- rep(1, length(x))
    for (i in 2:(length(cut) - 1))
        out[which(x > cut[i] & x <= cut[(i + 1)])] <- i
    return(out)}
## The hierarchy of sample aggregation
levsm <- data.frame(
    l1=cutter(mite.xy$y, cut = seq(0, 10, by = 10)),
    l2=cutter(mite.xy$y, cut = seq(0, 10, by = 5)),
    l3=cutter(mite.xy$y, cut = seq(0, 10, by = 2.5)),
    l4=1:nrow(mite))
## Let's see in a map
par(mfrow=c(1,3))
plot(mite.xy, main="l1", col=levsm$l1+1)
plot(mite.xy, main="l2", col=levsm$l2+1)
plot(mite.xy, main="l3", col=levsm$l3+1)
par(mfrow=c(1,1))
## Additive diversity partitioning
adpMite1 <- adipart(mite, levsm, index=c("ri", "sh", "si"), times=25)
adpMite1
summary(adpMite1)
plot(adpMite1)
## "Twoway" partitioning (maybe meaningless...)
substrate <- mite.env$Substrate
levels(substrate) <- c("sphagn", "sphagn", "sphagn", "sphagn", "other", "other", "other")
adpMite2 <- adipart(mite, levsm, substrate, index="sh", times=25)
adpMite2
summary(adpMite2)
plot(adpMite2)
## Simple artificial example
set.seed(4321)
matr <- r2dtable(1, c(3,4,3,7,4,8,7,6), 3:9)[[1]]
habitat <- c("a","a","a","b","a","b","b","b")
strata <- cbind(
   rep(1,8),
   rep(1:4,each=2),
   1:8)
x1 <- adipart(matr, strata, index=c("ri", "sh", "si"))
x1
summary(x1)
plot(x1)
x2 <- adipart(matr, strata, habitat)
x2
summary(x2)
plot(x2)
}
\keyword{multivariate}
