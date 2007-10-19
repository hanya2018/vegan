"print.oecosimu" <-
function(x, ...)
{
    cat("oecosimu with", length(x$oecosimu$simulated), "simulations\n")
    cat("simulation method", x$oecosimu$method)
    if (length(att <- attributes(x$oecosimu$simulated)) > 0) {
        cat(" with", paste(names(att), att, collapse=", "))
    }
    cat("\n\n")
    cat("summary of simulations:\n")
    sum <- sort(c(x$oecosimu$statistic, summary(x$oecosimu$simulated)))
    print(sum, ...)
    cat("\nz-value of the statistic:", format(x$oecosimu$z), "\n\n")
    NextMethod("print", x)
}
