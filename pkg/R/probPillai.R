`probPillai` <-
    function(Y, X, n, S11.inv, S22.inv, s, df1, df2, epsilon,
             Fref, nperm, ...)
##
## Permutation test for Pillai's trace in CCorA.
## Reference: Brian McArdle's unpublished graduate course notes.
##
##           Pierre Legendre, October 2007
{
    nGE <- 1
    for(i in 1:nperm) {
        Y.per <- Y[permuted.index(n, ...),, drop=FALSE]
        S12.per <- cov(Y.per,X)
        gross.mat <- S12.per %*% S22.inv %*% t(S12.per) %*% S11.inv
        Pillai.per <- sum(diag(gross.mat))
        Fper  <- (Pillai.per*df2)/((s-Pillai.per)*df1)
        if(Fper >= (Fref-epsilon)) nGE <- nGE+1
    }
    P <- nGE/(nperm+1)
}	                      
