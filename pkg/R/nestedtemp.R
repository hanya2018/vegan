`nestedtemp` <-
    function(comm, ...)
{
    .NotYetImplemented()
    comm <- ifelse(comm > 0, 1, 0)
    rs <- rowSums(comm)
    cs <- colSums(comm)
    r <- (rank(-rs, ties="aver") - 0.5)/(nrow(comm))
    c <- (rank(-cs, ties="aver") - 0.5)/(ncol(comm))
    dis <- outer(r, c, pmin)
    totdis <- 1 - abs(outer(r, c, "-"))
    fill <- sum(comm)/prod(dim(comm))
    ## Move each point to a diagonal, xy will be the x-coordinate
    xy <- (outer(r, c, "+") - 1)/2
    xy <- sweep(-xy, 1, r, "+")
    ## Fill line as a parabola against the diagonal. The argument is
    ## 0..1 (x-coordinate) instead of diagonal value 0..sqrt(2).
    ## fill is found from the parent environment.
    if (fill < 1/6) {
        ## If fill < 1/6, parabola will go over the borders
        parfun <- function(x) pmin((0.5-fill)*x, (1-x)*(0.5-fill))*2
    } else {
        ## The equation below really is a parabola, but in Horner form.
        parfun <- function(x) {
            out <- 3*(0.5-fill)*sqrt(2)*x*(1-x)
            out/sqrt(2)
        }
    }
    out <-  pmin(xy, 1-xy) - parfun(xy)
    ## Filline
    x <- seq(0,1,len=21)
    xline <- parfun(x)
    smo <- list(x = x - xline, y = (1-x) - xline)
    u <- (dis - out)/totdis
    u[u < 0 & comm == 1] <- 0
    u[u > 0 & comm == 0] <- 0
    u <- u^2
    colnames(u) <- colnames(comm)
    rownames(u) <- rownames(comm)
    temp <- 100*sum(u)/prod(dim(comm))/0.04145
    i <- rev(order(rs))
    j <- rev(order(cs))
    out <- list(comm = comm[i,j], u = u[i,j], r = r[i], c = c[j], 
                fill=fill,  statistic = temp, smooth=smo)
    class(out) <- "nestedtemp"
    out
}
