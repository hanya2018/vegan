`permCheck` <- function(object, control = permControl())
{
    ## if object is numeric or integer and of length 1,
    ## extend the object
    if(length(object) == 1 &&
       (is.integer(object) || is.numeric(object)))
        object <- seq_len(object)
    ## check the number of observations in object
    nobs <- getNumObs(object)
    ## if strata, check nobs == length of strata
    ## but beware empty levels
    if(!is.null(control$strata)) {
        tab <- table(control$strata)
        if(!identical(as.integer(nobs), as.integer(sum(tab))))
            stop("Number of observations and length of 'strata' do not match.")
        ## if "grid", check design balanced?
        if((bal <- length(unique(tab))) > 1 && control$type == "grid")
            stop("Unbalanced 'grid' designs are not supported.")
        ## if constant, check design balanced?
        if(control$constant && bal > 1)
            stop("Unbalanced designs not allowed with 'constant = TRUE'.")
        ## if permuting strata, must be balanced
        if(control$type == "strata" && bal > 1)
            stop("Design must be balanced if permuting 'strata'.")
    }
    ## get number of possible permutations
    num.pos <- numPerms(object, control)
    if(num.pos < control$minperm) {
        control$nperm <- control$maxperm <- num.pos
        control$complete <- TRUE
    }
    retval <- list(n = num.pos, control = control)
    class(retval) <- "permCheck"
    retval
}
