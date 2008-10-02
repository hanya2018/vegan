"print.radfit.frame" <-
    function (x, ...) 
{
    cat("\nDeviance for RAD models:\n\n")
    out <- sapply(x, function(x) unlist(lapply(x$models, deviance)))
    colnames(out) <- colnames(out, do.NULL = FALSE, prefix = "row")
    printCoefmat(out, na.print = "", ...)
    invisible(x)
}
