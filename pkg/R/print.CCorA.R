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
    out <- rbind("Eigen Values" = x$EigenValues, "Canonical Correlations" = x$CanCorr,
                 "R squares" = x$RDA.Rsquares, "adj. R squares" = x$RDA.adj.Rsq)
    colnames(out) <- colnames(x$AA)
    printCoefmat(out, ...)
    cat("\n")
    invisible(x) 
}
