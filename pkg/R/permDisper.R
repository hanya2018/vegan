`permDisper` <- function(object, pairwise = FALSE,
                         control = permControl(nperm = 999))
{
    t.statistic <- function(x, y) {
        m <- length(x)
        n <- length(y)
        xbar <- sum(x) / m
        ybar <- sum(y) / n
        #xvar <- var(x)
        #yvar <- var(y)
        xvar <- .Internal(cov(x, NULL, 1, FALSE))
        yvar <- .Internal(cov(y, NULL, 1, FALSE))
        pooled <- sqrt(((m-1)*xvar + (n-1)*yvar) / (m+n-2))
        (xbar - ybar) / (pooled * sqrt(1/m + 1/n))
    }
    if(!inherits(object, "betadisper"))
        stop("Only for class \"betadisper\"")
    nobs <- length(object$distances)
    mod <- lm(object$distances ~ object$group)
    mod.Q <- mod$qr
    p <- mod.Q$rank
    resids <- qr.resid(mod.Q, object$distances)
    res <- numeric(length = control$nperm + 1)
    res[1] <- summary(mod)$fstatistic[1]
    ## pairwise comparisons
    if(pairwise) {
        ## unique pairings
        combin <- combn(levels(object$group), 2)
        n.pairs <- ncol(combin)
        t.stats <- matrix(0, ncol = n.pairs, nrow = control$nperm + 1)
        t.stats[1,] <- apply(combn(levels(object$group), 2), 2, function(x) {
            t.statistic(object$distances[object$group == x[1]],
                        object$distances[object$group == x[2]])})
    }
    for(i in seq(along = res[-1])) {
        perm <- permuted.index2(nobs, control = control)
        perm.resid <- resids[perm]
        f <- qr.fitted(mod.Q, perm.resid)
        mss <- sum((f - mean(f))^2)
        r <- qr.resid(mod.Q, perm.resid)
        rss <- sum(r^2)
        rdf <- nobs - p
        resvar <- rss / rdf
        res[i+1] <- (mss / (p - 1)) / resvar
        ## pairwise comparisons
        if(pairwise) {
            for(j in seq_len(n.pairs)) {
                grp1 <- object$distance[perm][object$group == combin[1, j]]
                grp2 <- object$distance[perm][object$group == combin[2, j]]
                t.stats[i+1, j] <- t.statistic(grp1, grp2)
            }
        }
    }
    pval <- sum(res >= res[1]) / length(res)
    if(pairwise) {
        df <- apply(combin, 2, function(x) {
            length(object$distances[object$group == x[1]]) +
                length(object$distance[object$group == x[2]]) - 2})
        pairwise <- list(observed = 2 * pt(-abs(t.stats[1,]), df),
                         permuted = apply(t.stats, 2,
                         function(x) sum(abs(x) >= x[1])/length(x)))
        names(pairwise$observed) <- names(pairwise$permuted) <-
            apply(combin, 2, paste, collapse = "-")
    } else {
        pairwise <- NULL
    }
    mod.aov <- anova(object)
    retval <- cbind(mod.aov[, 1:4], c(control$nperm, NA), c(pval, NA))
    dimnames(retval) <- list(c("Groups", "Residuals"),
                             c("Df", "Sum Sq", "Mean Sq", "F", "N.Perm",
                               "Pr(>F)"))
    retval <- list(tab = retval, pairwise = pairwise,
                   groups = levels(object$group), control = control)
    class(retval) <- "permDisper"
    retval
}
