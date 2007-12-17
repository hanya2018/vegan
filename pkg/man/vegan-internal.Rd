\name{vegan-internal}
\alias{ordiGetData}
\alias{ordiParseFormula}
\alias{permuted.index}
\alias{centroids.cca}
\alias{ordiTerminfo}
\alias{ordispantree}
\alias{pasteCall}
% for new permutation schemes
\alias{permuted.index2}
\alias{permuted.series}
\alias{permuted.grid}
\alias{permuted.strata}
\alias{permControl}

\title{Internal vegan functions}

\description{
  Internal vegan functions.
}
\usage{
ordiGetData(call, env)
ordiParseFormula(formula, data, xlev = NULL)
ordiTerminfo(d, data)
centroids.cca(x, mf, wt)
permuted.index(n, strata)
ordispantree(ord, tree, display = "sites", ...)
pasteCall(call, prefix = "Call:")
}

\details{
  These are not to be called by the user. Function \code{ordispantree}
  is deprecated: use \code{\link{lines.spantree}} instead.
}

\keyword{internal }

