# Extract eigenvalues from an object that has them

`eigenvals` <-
    function(x, ...)
{
    UseMethod("eigenvals")
}

`eigenvals.default`<-
    function(x, ...)
{
    ## svd and eigen return unspecified 'list', see if this could be
    ## either of them
    out <- NA
    if (is.list(x)) {
        ## eigen
        if (length(x) == 2 && all(names(x) %in% c("values", "vectors")))
            out <- x$values
        ## svd: return squares of singular values
        if (length(x) == 3 && all(names(x) %in% c("d", "u", "v")))
            out <- x$d^2
    }
    class(out) <- "eigenvals"
    out
}

## squares of sdev 
`eigenvals.prcomp` <-
    function(x, ...)
{
    out <- x$sdev^2
    names(out) <- colnames(x$rotation)
    class(out) <- "eigenvals"
    out
}

## squares of sdev
`eigenvals.princomp` <-
    function(x, ...)
{
    out <- x$sdev^2
    class(out) <- "eigenvals"
    out
}

## concatenate constrained and unconstrained eigenvalues in cca, rda
## and capscale (vegan) -- ignore pCCA component
`eigenvals.cca` <- function(x, constrained = FALSE, ...)
{
   if (constrained)
       out <- x$CCA$eig
   else
       out <- c(x$CCA$eig, x$CA$eig)
   if (!is.null(out))
       class(out) <- c("eigenvals")
   out
}

## wcmdscale (in vegan)
`eigenvals.wcmdscale` <-
    function(x, ...)
{
    out <- x$eig
    class(out) <-"eigenvals"
    out
}

`print.eigenvals` <-
    function(x, ...)
{
    print(unclass(x), ...)
    invisible(x)
}

`summary.eigenvals` <-
    function(object, ...)
{
    vars <- object/sum(object)
    importance <- rbind(`Eigenvalue` = object,
                        `Proportion Explained` = round(vars, 5),
                        `Cumulative Proportion`= round(cumsum(vars), 5))
    out <- list(importance = importance)
    class(out) <- c("summary.eigenvals", "summary.prcomp")
    out
}
