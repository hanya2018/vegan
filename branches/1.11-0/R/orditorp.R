`orditorp` <-
    function (x, display, labels, choices = c(1, 2), priority,
              cex = 0.7, pcex, col = par("col"),
              pcol, pch = par("pch"), air = 1, ...) 
{
    if (missing(pcex)) 
        pcex <- cex
    if (missing(pcol)) 
        pcol <- col
    ## currently need to extract three arguments from '...'
    ## i) scaling, ii) origin and iii) shrink
    ## define local functions as per plot.default
    ## arguments after '...' in local functions are dropped and not passed on
    ##
    ## For future, if new scores methods accept extra arguments, add their
    ## names to the local functions below.
    localPoints <- function(..., shrink, origin, scaling) points(...)
    localText <- function(..., shrink, origin, scaling) text(...)
    x <- scores(x, display = display, choices = choices, ...)
    if (missing(labels)) 
        labels <- rownames(x)
    if (missing(priority)) 
        priority <- rowSums((scale(x)^2))
    w <- strwidth(labels, cex = cex)/2 * air
    h <- strheight(labels, cex = cex)/2 * air
    xx <- cbind(x[, 1] - w, x[, 1] + w, x[, 2] - h, x[, 2] + 
                h)
    is.na(priority) <- w == 0
    ord <- rev(order(priority, na.last = FALSE))
    xx <- xx[ord, ]
    x <- x[ord, ]
    labels <- labels[ord]
    tt <- logical(nrow(xx))
    tt[1] <- TRUE
    for (i in 2:(nrow(xx) - sum(is.na(priority)))) {
        j <- 1:(i - 1)
        j <- j[tt[j]]
        tt[i] <- all(xx[i, 1] > xx[j, 2] | xx[j, 1] > xx[i, 2] | 
                     xx[i, 3] > xx[j, 4] | xx[j, 3] > xx[i, 4])
    }
    if (sum(!tt)) {
        if (length(pch) > 1) 
            pch <- (pch[ord])[!tt]
        if (length(pcex) > 1) 
            pcex <- (pcex[ord])[!tt]
        if (length(pcol) > 1) 
            pcol <- (pcol[ord])[!tt]
        ##points(x[!tt, , drop = FALSE], pch = pch, cex = pcex, 
        ##       col = pcol, ...)
        localPoints(x[!tt, , drop = FALSE], pch = pch, cex = pcex, 
                    col = pcol, ...)
    }
    if (length(cex) > 1) 
        cex <- (cex[ord])[tt]
    if (length(col) > 1) 
        col <- (col[ord])[tt]
    ##text(x[tt, , drop = FALSE], labels[tt], cex = cex, col = col, 
    ##     ...)
    localText(x[tt, , drop = FALSE], labels[tt], cex = cex, col = col, 
              ...)
    names(tt) <- labels
    tt <- tt[order(ord)]
    invisible(tt)
}
