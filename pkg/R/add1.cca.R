`add1.cca`<-
    function(object, scope, test = c("none", "permutation"),
             perm.max = 200, ...)
{
    test <- match.arg(test)
    ## Default add1
    out <- NextMethod("add1", object, test = "none", ...)
    cl <- class(out)
    ## Loop over terms in 'scope' and do anova.cca
    if (test == "permutation") {
        if (!is.character(scope)) 
            scope <- add.scope(object, update.formula(object, scope))
        ns <- length(scope)
        adds <- matrix(0, ns+1, 3)
        adds[1, ] <- NA
        for (i in 1:ns) {
            tt <- scope[i]
            if (!is.null(object$CCA))
                nfit <- update(object,
                               as.formula(paste(". ~  Condition(.) + ", tt)))
            else
                nfit <- update(object,
                               as.formula(paste(". ~ . +", tt)))
            tmp <- anova(nfit, perm.max = perm.max, ...)
            adds[i+1,] <- unlist(tmp[1,3:5])
        }
        colnames(adds) <- colnames(tmp)[3:5]
        out <- cbind(out, adds)
        class(out) <- cl
    }
    out
}
