`print.CCorA` <-
    function(x, ...)
{
    cat("\nCanonical Correlation Analysis\n")
    cat("\nCall:\n")
    cat(deparse(x$call), "\n\n")
    out <- structure(rbind(x$Mat.ranks), dimnames = list("Matrix Ranks", c("Y", "X")))
    print(out, ...)
    cat("\n")
    cat("Pillai's trace: ", format(x$Pillai, ...), "\n")
    cat("\n")
    cat("Significance of Pillai's trace:\n")
    if (x$nperm > 0) {
        cat("based on", x$nperm, "permutations: ")
        cat(x$p.perm,"\n")
    }
    if(is.na(x$p.Pillai)) {
       cat("Parametric probability not computed with covariables", "\n")
       cat("Use the results of the permutation test", "\n\n")
       } else {
       cat("from F-distribution: ", format.pval(x$p.Pillai), "\n\n")       
       }
    out <- rbind("Eigenvalues" = x$EigenValues, "Canonical Correlations" = x$CanCorr)
    colnames(out) <- colnames(x$AA)
    printCoefmat(out, ...)
    cat("\n")
    out <- rbind("RDA R squares" = x$RDA.Rsquares, "adj. RDA R squares" = x$RDA.adj.Rsq)
    colnames(out) <- c("Y | X", "X | Y")
    printCoefmat(out, ...)
    cat("\n")
    invisible(x) 
}
