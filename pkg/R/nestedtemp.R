"nestedtemp" <-
function(comm, ...)
{
    comm <- ifelse(comm > 0, 1, 0)
    i <- rev(order(rowSums(comm)))
    j <- rev(order(colSums(comm)))
    comm <- comm[i,j]
    fill <- sum(comm)/prod(dim(comm))
    r <- seq(0, 1, length=nrow(comm))
    c <- seq(0, 1, length=ncol(comm))
    dis <- outer(r, c, pmin)
    totdis <- 1 - abs(outer(r, c, "-"))
    u <- ((dis - fill*totdis)/totdis)^2
    u[is.na(u)] <- 0
    u[dis < fill*totdis & comm == 1] <- 0
    u[dis >= fill*totdis & comm == 0] <- 0
    colnames(u) <- colnames(comm)
    rownames(u) <- rownames(comm)
    temp <- 100*sum(u)/prod(dim(comm))/0.04145
    out <- list(comm = comm, u = u, fill=fill,  statistic = temp)
    class(out) <- "nestedtemp"
    out
}

