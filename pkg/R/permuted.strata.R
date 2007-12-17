`permuted.strata` <- function(strata)
{
    lev <- length(levels(strata))
    ngr <- length(strata) / lev
    rep(sample(lev), ngr) + (rep(seq(0, ngr-1), each = lev) * lev)
}
