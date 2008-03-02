`nestedtemp` <-
    function(comm, ...)
{
    ## J Biogeogr 33, 924-935 (2006) says that Atmar & Patterson try
    ## to pack presences and absence to minimal matrix temperature,
    ## and the following routines try to reproduce the (partly verbal)
    ## description. Index s should pack ones, and index t should pack
    ## zeros, and the final ordering should be "a compromise".
    colpack <- function(x, rr)
    {
        ind <- matrix(rep(rr, ncol(x)), nrow=nrow(x))
        s <- rank(-colSums((x*ind)^2), ties="aver")
        t <- rank(-colSums((nrow(x) - (1-x)*ind + 1)^2), ties="aver")
        st <- rank(s+t, ties="random")
        st
    }
    rowpack <- function(x, cr)
    {
        ind <- matrix(rep(cr, each=nrow(x)), nrow=nrow(x))
        s <- rank(-rowSums((x*ind)^2), ties="aver")
        t <- rank(-rowSums((ncol(x) - (1-x)*ind + 1)^2), ties="aver")
        st <- rank(s+t, ties="random")
        st
    }
    comm <- ifelse(comm > 0, 1, 0)
    ## Start with columns, expect if nrow > ncol
    if (ncol(comm) >= nrow(comm)) {
        i <- rank(-rowSums(comm), ties="average")
    } else {
        j <- rank(-colSums(comm), ties="average")
        i <- rowpack(comm, j)
    }
    ## Improve eight times
    for (k in 1:8) {
        j <- colpack(comm, i)
        i <- rowpack(comm, j)
    }
    if (ncol(comm) < nrow(comm))
        j <- colpack(comm, i)
    comm <- comm[order(i), order(j)]
    r <- ppoints(nrow(comm), a=0.5)
    c <- ppoints(ncol(comm), a=0.5)
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
    out <- list(comm = comm, u = u, r = r, c = c, 
                fill=fill,  statistic = temp, smooth=smo)
    class(out) <- "nestedtemp"
    out
}

