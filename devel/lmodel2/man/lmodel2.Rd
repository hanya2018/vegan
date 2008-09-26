\encoding{UTF-8}
\name{lmodel2}
\alias{lmodel2}
\alias{plot.lmodel2}
\alias{lines.lmodel2}
\alias{print.lmodel2}
\alias{MA.reg}
\alias{SMA.reg}
\alias{CLma}
\alias{CLsma}
\alias{permutest.lmodel2}

\title{Model II regression}

\description{ This function computes model II simple linear
regression using the following methods: ordinary least squares (OLS),
major axis (MA), standard major axis (SMA), and ranged major axis
(RMA). The model only accepts one response and one explanatory
variable. }

\usage{
lmodel2(formula, data = NULL, range.y=NULL, range.x=NULL, nperm=0,
   control = permControl(nperm=nperm))
}

\arguments{
  \item{formula }{ A formula specifying the bivariate model, as in
  \code{\link{lm}} and \code{\link{aov}}. }
  \item{data}{ A data frame containing the two variables specified in the formula. }
  \item{range.y, range.x}{ Parametres for ranged major axis regression
  (RMA). If \code{range.y = NULL} and \code{range.x = NULL}, RMA will
  not be computed. If only one of them is \code{NULL}, the program will stop.  
  If \code{range.y = "relative"}: variable \code{y} has a true zero
  (relative-scale variable). If \code{range.y = "interval"}: variable
  \code{y} possibly includes negative values (interval-scale
  variable). If \code{range.x = "relative"}: variable \code{x} has a
  true zero (relative-scale variable). If \code{range.x = "interval"}:
  variable \code{x} possibly includes negative values (interval-scale
  variable)  }
  \item{nperm}{ Number of permutations for the tests. If \code{nperm =
   0}, tests will not be computed. } 
   \item{control}{Permutation control item for
  \code{\link[vegan]{permuted.index2}}.} 
} 

\details{
  Model II regression should be used when the two variables in the
  regression equation are random, i.e. not controlled by the
  researcher. Model I regression using least squares underestimates
  the slope of the linear relationship between the variables when they
  both contain error. Ordinary least squares (OLS) is, however, appropriate 
  in some cases as a model II regression model; see the \dQuote{Model II User's guide, 2008}.

  The model II regression methods of ordinary least squares (OLS),
  major axis (MA), standard major axis (SMA), and ranged major axis
  (RMA) are described in Legendre and Legendre (1998, Section
  10.3.2). OLS, MA, and SMA are also described in Sokal and Rohlf
  (1995). The PDF document \dQuote{Model II User's guide, 2008}
  provided with this function contains a tutorial for model II
  regression.  
  
  The \code{plot} function plots the data points together with one of the
  regression lines, specified by \code{method="OLS"}, \code{method="MA"} (default),
  \code{method="SMA"}, or \code{method="RMA"}, and its 95 percent confidence interval.
}

\value{
  The default output provides the regression output. It draws
  information from a list, produced by function \code{lmodel2}, which
  contains the following elements: 
  \item{ y }{ The response variable.  }
  \item{ x }{ The explanatory variable. }
  \item{ regression.results }{ A table with rows corresponding to the
  four regression methods. Column 1 gives the method name, followed by
  the intercept and slope estimates, the angle between the regression
  line and the abscissa, and the permutational probability
  (one-tailed, for the tail corresponding to the sign of the slope
  estimate). } 
  \item{ confidence.intervals }{ A table with rows corresponding to
  the four regression methods. The method name is followed by the
  parametric 95% confidence intervals (2.5 and 97.5 percentiles) for
  the intercept and slope estimates. } 
 \item{ eigenvalues  }{ Eigenvalues of the bivariate dispersion,
  computed during major axis regression. } 
  \item{ H }{ The H statistic used for computing the confidence
  interval of the major axis slope. Notation following Sokal and Rohlf
  (1995). } 
  \item{ n }{ Number of objects. }
  \item{ r }{ Correlation coefficient. }
  \item{ rsquare }{ Coefficient of determination (R-square) of the OLS regression. }
  \item{ P.param }{ 2-tailed parametric P-value for the test of r and the OLS slope. }
  \item{ theta }{ Angle between the two OLS regression lines,
  \code{lm(y ~ x)} and \code{lm(x ~ y)}.  }
  \item{ nperm }{ Number of permutations for the permutation tests. }
  \item{ epsilon }{ Any value smaller than epsilon is considered to be zero. }
  \item{ info.slope }{ Information about the slope notation when \eqn{r = 0}. }
  \item{ info.CI }{ Information about the confidence limits notation
  when the slope is infinite. } 
  \item{ call }{ Call of the function. }
} 

\references{ 
Legendre, P. and L. Legendre. 1998. Numerical ecology, 2nd English
edition. Elsevier Science BV, Amsterdam. 

Sokal, R. R. and F. J. Rohlf. 1995. Biometry -- The principles and
practice of statistics in biological research. 3rd
edition. W. H. Freeman, New York. 
}

\seealso{ 
A tutorial (file \dQuote{Model II User's guide, 2008}) is provided with this function.
}

\author{ Pierre Legendre, Departement de Sciences Biologiques, Universite de Montreal }

\examples{
\dontrun{
## The example data files are described in more detail in the \dQuote{Model II User's guide, 2008} tutorial.

# Example 1, manual p. 8-10 (surgical unit data)

survival = c(2.0326, 2.4086, 2.2177, 1.9078, 2.0035, 2.0945, 1.7652, 1.7925, 2.1292, 2.2295, 2.1524, 2.3188, 1.9039, 2.0508, 2.6525, 2.2053, 1.9246, 2.1541, 2.4970, 1.7237, 2.8339, 2.1282, 2.6884, 2.4284, 2.0261, 2.0843, 2.2826, 2.2073, 2.0443, 2.4863, 1.9037, 2.6647, 1.9071, 1.9093, 2.4389, 2.3343, 1.3379, 2.1996, 1.8795, 2.1504, 1.4330, 2.4381, 2.1075, 2.2843, 2.1615, 2.0558, 2.7249, 2.0520, 2.6810, 2.2604, 2.2553, 2.1745, 2.0224, 2.1413)
predicted = c(2.09801, 2.32720, 1.73399, 2.08059, 2.09162, 2.14018, 1.85363, 1.83000, 1.98995, 2.16076, 2.28290, 2.30727, 1.78237, 2.01970, 2.50291, 2.22350, 2.07568, 2.10552, 2.14815, 2.00219, 2.70781, 2.24899, 2.40337, 2.36723, 1.88477, 1.78747, 2.13110, 1.88625, 2.18272, 2.25334, 1.87875, 2.40431, 2.10274, 2.00209, 2.42302, 2.29439, 1.48423, 2.03926, 1.84084, 1.97356, 1.70035, 2.38549, 2.10302, 2.37947, 2.22517, 1.93852, 2.40569, 2.02341, 2.51570, 2.10089, 2.03573, 1.96179, 2.04167, 2.01572)
Ex1 = as.data.frame(cbind(survival,predicted))
colnames(Ex1) = c("Survival", "Predicted_by_model")
Ex1.res = lmodel2(Predicted_by_model ~ Survival, data=Ex1, nperm=199)
Ex1.res
# Equivalent formulation: 
Ex1.res = lmodel2(Ex1[,2] ~ Ex1[,1], nperm=199)
print(Ex1.res)
plot(Ex1.res) 
# Equivalent formulation (method="MA" is the default for plots): 
plot(Ex1.res, "MA")

# Example 2, manual p. 11-14 (eagle rays and Macomona)

predators = c(2, 11, 3, 5, 8, 8, 4, 15, 8, 7, 3, 13, 8, 18, 9, 6, 2, 19, 4, 14)
prey = c(29, 53, 18, 27, 29, 41, 29, 58, 49, 31, 33, 71, 53, 59, 33, 42, 20, 71, 40, 54)
Ex2 = as.data.frame(cbind(predators,prey))
colnames(Ex2) = c("Predators", "Prey")
Ex2.res = lmodel2(Prey ~ Predators, data=Ex2, "relative", "relative", 199)
# Equivalent formulation: 
Ex2.res = lmodel2(Ex2[,2] ~ Ex2[,1], range.y="relative", range.x="relative", nperm=199)
print(Ex2.res)
par(mfrow = c(1,2))
plot(Ex2.res, "SMA")
plot(Ex2.res, "RMA")

# Example 3, manual p. 14-17 (cabezon spawning)

mass = c(14, 17, 24, 25, 27, 33, 34, 37, 40, 41, 42)
eggs = c(61, 37, 65, 69, 54, 93, 87, 89, 100, 90, 97)
Ex3 = as.data.frame(cbind(mass,eggs))
colnames(Ex3) = c("Mass", "Eggs")
Ex3.res = lmodel2(Eggs ~ Mass, data=Ex3, "relative", "relative", 199)
print(Ex3.res)
par(mfrow = c(1,2))
plot(Ex3.res, "SMA")
plot(Ex3.res, "RMA")

# Example 4, manual p. 17-19 (highly correlated random variables)

x = c(5.831, 8.421, 8.737, 7.454, 5.819, 7.335, 8.569, 0.694, 4.063, 0.981, 1.493, 8.368, 6.480, 8.480, 1.488, 9.573, 2.738, 0.454, 5.423, 5.420, 1.033, 2.303, 5.956, 3.134, 9.468, 4.783, 1.980, 5.660, 6.566, 4.938, 4.463, 7.292, 3.154, 0.948, -0.787, 3.748, 5.465, 8.442, 5.984, 0.141, 3.954, 9.303, 7.450, 10.491, 4.892, 2.703, 4.365, 10.123, 2.329, 3.713, 6.520, 7.005, 5.976, 6.333, 1.028, 2.220, 1.568, 0.892, 8.137, 3.142, 6.974, 1.663, 7.193, 6.639, 9.625, 0.548, 7.527, -0.811, 7.882, 7.675, 4.129, 9.448, 10.071, -0.770, 4.975, 8.641, 3.831, 2.098, 0.814, 4.443, 6.997, 7.547, 4.115, 9.043, 6.707, 7.858, 1.034, 2.786, 7.835, 8.495, 0.844, 6.969, 1.991, 5.333, 0.437, 4.160, 9.122, 1.187, 10.782, 3.445)
y = c(5.055, 9.404, 10.568, 8.542, 4.234, 6.589, 7.217, 1.971, 4.269, 1.890, 2.799, 9.918, 5.825, 9.158, 2.559, 7.172, 2.643, 1.840, 4.115, 4.706, 2.784, 2.921, 5.792, 3.865, 6.966, 4.653, 2.692, 6.350, 6.127, 4.362, 1.101, 9.402, 2.471, 1.493, -1.514, 6.382, 4.790, 7.754, 5.495, 1.887, 1.967, 10.452, 9.654, 9.430, 4.775, 5.195, 6.390, 10.350, 2.947, 3.770, 7.863, 10.586, 7.292, 6.304, 1.463, 0.811, 2.614, 1.086, 8.618, 3.568, 9.131, 4.068, 6.078, 6.750, 8.793, 0.840, 5.444, 1.135, 5.709, 7.868, 3.337, 9.234, 10.305, -0.477, 5.694, 7.840, 2.650, 0.988, 2.007, 5.100, 7.248, 7.398, 5.341, 7.254, 6.709, 8.393, 0.188, 1.092, 6.973, 7.105, 3.577, 6.279, 1.207, 4.084, 1.206, 1.851, 8.467, 0.931, 7.439, 6.350)
Ex4 = as.data.frame(cbind(x,y))
colnames(Ex4) = c("x", "y")
Ex4.res = lmodel2(y ~ x, data=Ex4, "interval", "interval", 199)
print(Ex4.res)
par(mfrow = c(1,2))
plot(Ex4.res, "OLS")
plot(Ex4.res, "MA")

# Example 5, manual p. 19-22 (uncorrelated random variables)

random_x = c(0.50324, -0.18863, 0.83679, 2.39702, 1.09035, -0.06973, 0.41723, 0.98478, -0.03663, 0.26074, 0.70153, 0.86720, 0.23300, -0.03791, 1.04477, 1.21983, 0.69904, 0.67740, 1.18361, -0.15731, 0.15497, 0.27924, 0.58255, -0.51545, 1.55933, 0.88662, -0.23775, -0.53665, 1.32041, -0.07095, -0.33037, -1.09646, -1.52390, -1.39687, -0.90181, 2.05918, -0.69675, 0.87076, -1.51449, 0.78878, 0.03520, 0.71277, -1.02313, 0.89874, 0.84861, 1.99309, 1.04455, -0.95050, -1.30776, 0.20376, 0.99486, -0.58439, -0.79658, -0.92957, 0.41722, 1.76177, 0.84389, -1.48351, 0.19074, -0.14526, 1.53062, -0.34673, -0.50017, 1.25696, 0.16333, -0.34276, 0.13056, -1.66661, -1.37404, -0.78424, 1.34774, -0.32051, -0.85556, 0.96584, 0.95532, -0.60135, -1.51872, 1.13056, 0.29374, 0.61733, -0.23913, -0.36236, -0.15351, 1.10019, 0.82884, -2.48132, 1.92432, -0.80866, 0.48533, 0.11363, -2.66496, -0.58644, -1.52143, -1.12820, 0.58871, -0.60695, -0.13764, -1.09231, -0.91970, 0.66351)
random_y = c(1.93446, -1.98676, 0.54333, -0.95102, -0.65726, 1.31458, -0.40181, -0.32563, -0.27623, 1.38629, -1.59982, 0.67491, -0.99922, -0.13943, -0.56905, -0.55778, 0.41293, 1.05840, 0.27067, -0.08838, 1.23365, -0.52153, 0.72294, 1.41078, -1.65478, -1.47813, 0.07189, 0.10557, 0.03602, -0.36015, -1.27080, -0.47249, -1.10436, 0.56788, 0.75570, -0.78914, 1.31254, -1.64405, -0.20520, -0.59638, -0.38128, 1.51211, 0.46978, -0.49725, 1.13412, -0.38103, 0.85258, 1.29989, 0.94955, 2.35266, 2.01088, -0.75915, 1.37746, -0.80059, -0.30573, -1.15059, 0.40665, 0.86980, 0.95859, -0.02436, 0.62997, -0.20778, -0.09589, -0.45062, 0.38424, 0.86314, -0.74771, 0.23681, -0.89078, 1.19361, -0.41306, 0.69088, 0.03710, -1.46022, 0.53830, -1.49693, 1.01561, 0.32642, 1.12687, -0.52796, -1.61981, -1.50747, 0.34364, 1.11328, 1.19421, 1.63151, 0.49800, -1.20476, 0.12101, 0.98289, -1.08756, 0.12018, 0.73779, -1.61143, 0.72025, -0.33496, 0.87172, -0.18392, 0.08284, -0.25230)
Ex5 = as.data.frame(cbind(random_x,random_y))
colnames(Ex4) = c("random_x", "random_y")
Ex5.res = lmodel2(random_y ~ random_x, data=Ex5, "interval", "interval", 199)
print(Ex5.res)
par(mfrow = c(2,2))
plot(Ex5.res, "OLS")
plot(Ex5.res, "MA")
plot(Ex5.res, "SMA")
plot(Ex5.res, "RMA")

# Example 6 where cor(y,x) = 0 by construct (square grid of points)

y0 = rep(c(1,2,3,4,5),5)
x0 = c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5))
plot(x0, y0)
Ex6 = as.data.frame(cbind(x0,y0))
zero.res = lmodel2(y0 ~ x0, data=Ex6, "relative", "relative")
print(zero.res)
par(mfrow = c(1,2))
plot(zero.res, "OLS")
plot(zero.res, "MA")
}
}

\keyword{ models}
\keyword{regression}
