"print.permutest.cca" <-
    function (x, ...) 
{
    cat("\nPermutation test for", x$method, "\nCall:\n")
    cat(deparse(x$call), "\n\n")
    Pval <- sum(x$F.perm >= x$F.0)/x$nperm
    cat("Test for significance of all constrained eigenvalues\n")
    cat("Pseudo-F:\t", x$F.0, "\n")
    cat("Significance:\t", format.pval(Pval, eps = 1/x$nperm), 
        "\n")
    cat("Based on", x$nperm, "permutations under", x$model, "model")
    if (!is.null(x$strata)) 
        cat(",\nstratified within factor", x$strata)
    cat(".\n\n")
    invisible(x)
}
