`nestedtemp` <-
    function(comm, ...)
{
    comm <- ifelse(comm > 0, 1, 0)
    rs <- rowSums(comm)
    cs <- colSums(comm)
    r <- (rank(-rs, ties="aver") - 1)/(nrow(comm)-1)
    c <- (rank(-cs, ties="aver") - 1)/(ncol(comm)-1)
    dis <- outer(r, c, pmin)
    totdis <- 1 - abs(outer(r, c, "-"))
    fill <- sum(comm)/prod(dim(comm))
    u <- ((dis - fill*totdis)/totdis)^2
    u[is.na(u)] <- 0
    u[dis < fill*totdis & comm == 1] <- 0
    u[dis >= fill*totdis & comm == 0] <- 0
    colnames(u) <- colnames(comm)
    rownames(u) <- rownames(comm)
    temp <- 100*sum(u)/prod(dim(comm))/0.04145
    i <- rev(order(rs))
    j <- rev(order(cs))
    out <- list(comm = comm[i,j], u = u[i,j], fill=fill,  statistic = temp)
    class(out) <- "nestedtemp"
    out
}
