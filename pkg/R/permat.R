## permatfull function
`permatfull` <-
function(m, fixedmar="both", reg=NULL, hab=NULL, mtype="count", times=100)
{
    mtype <- match.arg(mtype, c("prab", "count"))
    count <- if (mtype == "count") TRUE else FALSE
    fixedmar <- match.arg(fixedmar, c("none", "rows", "columns", "both"))
    m <- as.matrix(m)
    n.row <- nrow(m)
    n.col <- ncol(m)
    if (mtype == "prab") m <- matrix(as.numeric(m > 0), n.row, n.col)
    if (is.null(reg) & is.null(hab)) str <- as.factor(rep(1, n.row))
    if (!is.null(reg) & is.null(hab)) str <- as.factor(reg)
    if (is.null(reg) & !is.null(hab)) str <- as.factor(hab)
    if (!is.null(reg) & !is.null(hab)) str <- interaction(reg, hab, drop=TRUE)
    levels(str) <- 1:length(unique(str))
    str <- as.numeric(str)
    nstr <- length(unique(str))
    if (any(tapply(str,list(str),length) == 1))
        stop("strata should contain at least 2 observations")
    perm <- list()
    for (k in 1:times)
        perm[[k]] <- matrix(0, n.row, n.col)
    for (j in 1:nstr) {
    id <- which(str == j)
        if (fixedmar == "none")
            for (i in 1:times)
                if (count) perm[[i]][id,] <- matrix(sample(m[id,]), length(id), n.col)
                else perm[[i]][id,] <- commsimulator(m[id,], method="r00")
        if (fixedmar == "rows")
            for (i in 1:times)
                if (count) perm[[i]][id,] <- apply(m[id,], 2, sample)
                else perm[[i]][id,] <- commsimulator(m[id,], method="r0")
        if (fixedmar == "cols")
            for (i in 1:times)
                if (count) perm[[i]][id,] <- t(apply(m[id,], 1, sample))
                else perm[[i]][id,] <- commsimulator(m[id,], method="c0")
        if (fixedmar == "both")
            for (i in 1:times)
                if (count) perm[[i]][id,] <- r2dtable(1, apply(m[id,], 1, sum), apply(m[id,], 2, sum))[[1]]
                else perm[[i]][id,] <- commsimulator(m[id,], method="quasiswap")
        }
    specs <- list(reg=reg, hab=hab)
    out <- list(call=match.call(), orig=m, perm=perm, specs=specs)
    attr(out, "mtype") <- mtype
    attr(out, "ptype") <- "full"
    attr(out, "fixedmar") <- fixedmar
    attr(out, "times") <- times
    class(out) <- c("permat", "list")
    return(out)
}

## permatswap function
`permatswap` <-
function(m, reg=NULL, hab=NULL, mtype="count", method="swap", times=100, burnin = 10000, thin = 1000)
{
    mtype <- match.arg(mtype, c("prab", "count"))
    count <- if (mtype == "count") TRUE else FALSE
    if (count) {
        method <- match.arg(method, "swap")
        } else {method <- match.arg(method, c("swap", "tswap", "backtrack"))}

    m <- as.matrix(m)
    n.row <- nrow(m)
    n.col <- ncol(m)
    if (mtype == "prab") m <- matrix(as.numeric(m > 0), n.row, n.col)
    if (is.null(reg) & is.null(hab)) str <- as.factor(rep(1, n.row))
    if (!is.null(reg) & is.null(hab)) str <- as.factor(reg)
    if (is.null(reg) & !is.null(hab)) str <- as.factor(hab)
    if (!is.null(reg) & !is.null(hab)) str <- interaction(reg, hab, drop=TRUE)
    levels(str) <- 1:length(unique(str))
    str <- as.numeric(str)
    nstr <- length(unique(str))
    if (any(tapply(str,list(str),length) == 1))
        stop("strata should contain at least 2 observations")
    perm <- list()
    for (i in 1:times)
        perm[[i]] <- matrix(0, n.row, n.col)

    for (j in 1:nstr) {
    id <- which(str == j)
    temp <- m[id,]
    if (count) for (k in 1:burnin) temp <- swapcount(temp)
        else for (k in 1:burnin) temp <- commsimulator(temp, method=method)
    for (i in 1:times)
        if (count) perm[[i]][id,] <- swapcount(temp)
        else perm[[i]][id,] <- commsimulator(temp, method=method, thin=thin)
        }
    specs <- list(reg=reg, hab=hab, burnin=burnin, thin=thin)
    out <- list(call=match.call(), orig=m, perm=perm, specs=specs)
    attr(out, "mtype") <- mtype
    attr(out, "ptype") <- "swap"
    attr(out, "fixedmar") <- "both"
    attr(out, "times") <- times
    class(out) <- c("permat", "list")
    return(out)
}

## S3 plot method for permat
`plot.permat` <-
function(x, ...)
{
    n <- attr(x, "times")
    bray <- numeric(n)
    for (i in 1:n) bray[i] <- sum(abs(x$orig-x$perm[[i]]))/sum(x$orig+x$perm[[i]])
    plot(bray,type="n",ylim=c(0,1),ylab="Bray-Curtis dissimilarity",xlab="Runs", ...)
    for (i in c(0.4, 0.6)) abline(i,0, lty=2, col="grey")
    lines(bray,col="red")
    lines(lowess(bray),col="blue",lty=2)
    title(sub=paste("(mean = ", substitute(z, list(z=round(mean(bray),3))), 
        ", min = ", substitute(z, list(z=round(min(bray),3))),
        ", max = ", substitute(z, list(z=round(max(bray),3))), ")", sep=""))
    invisible(NULL)
}

## S3 print method for permat
`print.permat` <-
function(x, digits=3, ...)
{
    if (attr(x, "ptype") != "sar" & !is.null(x$specs$reg) | !is.null(x$specs$hab))
        restr <- TRUE else restr <- FALSE
    cat("Object of class 'permat'\n\nCall: ")
    print(x$call)
    cat("Matrix type:", attr(x, "mtype"), "\nPermutation type:", attr(x, "ptype"))
    cat("\nRestricted:", restr, "\nFixed margins:", attr(x, "fixedmar"))
    cat("\n\nMatrix dimensions:", nrow(x$orig), "rows,", ncol(x$orig), "columns")
    cat("\nSum of original matrix:", sum(x$orig))
    cat("\nFill of original matrix:", round(sum(x$orig>0)/(nrow(x$orig)*ncol(x$orig)),digits))
    cat("\nNumber of permuted matrices:", length(x$perm),"\n")
}

## S3 summary method for permat
`summary.permat` <-
function(object, digits=2, ...)
{
    x <- object
    n <- attr(x, "times")
    if (attr(x, "ptype") != "sar" & !is.null(x$specs$reg) | !is.null(x$specs$hab))
        restr <- TRUE else restr <- FALSE
    if (restr) {
        if (!is.null(x$specs$reg) & is.null(x$specs$hab)) int <- x$specs$reg
        if (is.null(x$specs$reg) & !is.null(x$specs$hab)) int <- x$specs$hab
        if (!is.null(x$specs$reg) & !is.null(x$specs$hab))
            int <- interaction(x$specs$reg, x$specs$hab, drop=TRUE)
        ssum <- numeric(n)}
    bray <- psum <- pfill <- vrow <- vcol <- numeric(n)
    for (i in 1:n) {
        bray[i] <- sum(abs(x$orig-x$perm[[i]]))/sum(x$orig+x$perm[[i]])
        psum[i] <- sum(x$orig) == sum(x$perm[[i]])
        pfill[i] <- sum(x$orig > 0) == sum(x$perm[[i]] > 0)
        vrow[i] <- identical(rowSums(x$orig), rowSums(x$perm[[i]]))
        vcol[i] <- identical(colSums(x$orig), colSums(x$perm[[i]]))
        if (restr) ssum[i] <- identical(rowSums(aggregate(x$orig,list(x$specs$reg),sum)),
            rowSums(aggregate(x$perm[[i]],list(x$specs$reg),sum)))
        }
    strsum <- if (restr) sum(ssum)/n else NA
    outv <- c(sum=sum(psum)/n, fill=sum(pfill)/n, rowsums=sum(vrow)/n, colsums=sum(vcol)/n, strsum=strsum)

    cat("Summary of object of class 'permat'\n\nCall: ")
    print(x$call)
    cat("Matrix type:", attr(x, "mtype"), "\nPermutation type:", attr(x, "ptype"))
    cat("\nRestricted:", restr, "\nFixed margins:", attr(x, "fixedmar"))
    cat("\n\nMatrix dimensions:", nrow(x$orig), "rows,", ncol(x$orig), "columns")
    cat("\nSum of original matrix:", sum(x$orig))
    cat("\nFill of original matrix:", round(sum(x$orig>0)/(nrow(x$orig)*ncol(x$orig)),digits))
    cat("\nNumber of permuted matrices:", length(x$perm),"\n")

    cat("\nMatrix sums retained:", round(100*outv[1], digits), "%")
    cat("\nMatrix fill retained:", round(100*outv[2], digits), "%")
    cat("\nRow sums retained:   ", round(100*outv[3], digits), "%")
    cat("\nColumn sums retained:", round(100*outv[4], digits), "%")
    if (restr) cat("\nSums within strata retained:", round(100*outv[5], digits), "%")
    cat("\n\nBray-Curtis dissimilarities among original and permuted matrices:\n")
    print(summary(bray))
    out <- list(bray=bray, restr=outv)
    class(out) <- c("summary.permat", "list")
    invisible(out)
}
