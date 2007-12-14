`permControl` <- function(strata = NULL, type = c("free", "series", "grid"),
                          mirror = FALSE, constant = FALSE,
                          ncol = NULL, nrow = NULL)
{
    if(missing(type))
        type <- "free"
    else
        type <- match.arg(type)
    out <- list(strata = strata, type = type, mirror = mirror,
                constant = constant, ncol = ncol, nrow = nrow)
    class(out) <- "permControl"
    return(out)
}
