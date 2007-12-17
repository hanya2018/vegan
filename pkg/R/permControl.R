`permControl` <- function(strata = NULL, nperm = 199, maxperm = 9999,
                          type = c("free", "series", "grid", "strata"),
                          mirror = FALSE, constant = FALSE,
                          ncol = NULL, nrow = NULL)
{
    if(missing(type))
        type <- "free"
    else
        type <- match.arg(type)
    out <- list(strata = strata, nperm = nperm, maxperm = maxperm,
                type = type, mirror = mirror,
                constant = constant, ncol = ncol, nrow = nrow)
    class(out) <- "permControl"
    return(out)
}
