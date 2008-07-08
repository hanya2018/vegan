## permatfull function
`permatfull` <-
function(m, fixedmar="both", reg=NULL, hab=NULL, mtype="count", times=100)
{
    if (!identical(all.equal(m, round(m)), TRUE))
       stop("function accepts only integers (counts)")
    mtype <- match.arg(mtype, c("prab", "count"))
    count <- mtype == "count"
    fixedmar <- match.arg(fixedmar, c("none", "rows", "columns", "both"))
    m <- as.matrix(m)
    n.row <- nrow(m)
    n.col <- ncol(m)
    if (mtype == "prab") m <- matrix(as.numeric(m > 0), n.row, n.col)
    if (is.null(reg) && is.null(hab)) str <- as.factor(rep(1, n.row))
    if (!is.null(reg) && is.null(hab)) str <- as.factor(reg)
    if (is.null(reg) && !is.null(hab)) str <- as.factor(hab)
    if (!is.null(reg) && !is.null(hab)) str <- interaction(reg, hab, drop=TRUE)
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
        if (fixedmar == "columns")
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
    if (!identical(all.equal(m, round(m)), TRUE))
       stop("function accepts only integers (counts)")
    mtype <- match.arg(mtype, c("prab", "count"))
    count <- mtype == "count"
    if (count) {
        method <- match.arg(method, "swap")
    } else {method <- match.arg(method, c("swap", "tswap"))}

    m <- as.matrix(m)
    n.row <- nrow(m)
    n.col <- ncol(m)
    if (mtype == "prab") m <- matrix(as.numeric(m > 0), n.row, n.col)
    if (is.null(reg) && is.null(hab)) str <- as.factor(rep(1, n.row))
    if (!is.null(reg) && is.null(hab)) str <- as.factor(reg)
    if (is.null(reg) && !is.null(hab)) str <- as.factor(hab)
    if (!is.null(reg) && !is.null(hab)) str <- interaction(reg, hab, drop=TRUE)
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
        if (count)
            for (k in 1:burnin)
                temp <- .C("swapcount", m = as.double(temp),
                        as.integer(n.row), as.integer(n.col),
                        as.integer(1), PACKAGE = "vegan")$m
        else
            for (k in 1:burnin)
                temp <- commsimulator(temp, method=method)
        for (i in 1:times) {
            if (count)
                perm[[i]][id,] <- .C("swapcount",
                                  m = as.double(temp),
                                  as.integer(n.row),
                                  as.integer(n.col),
                                  as.integer(thin),
                                  PACKAGE = "vegan")$m
	    else perm[[i]][id,] <- commsimulator(temp, method=method, thin=thin)
            temp <- perm[[i]][id,]
        }
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
    plot(bray,type="n",ylab="Bray-Curtis dissimilarity",xlab="Runs", ...)
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
    cat("\nNumber of permuted matrices:", attr(x, "times"),"\n")
}

## S3 summary method for permat
`summary.permat` <-
function(object, ...)
{
    x <- object
    n <- attr(x, "times")
    if (attr(x, "ptype") != "sar" && !is.null(x$specs$reg) || !is.null(x$specs$hab))
        restr <- TRUE else restr <- FALSE
    if (restr) {
        if (!is.null(x$specs$reg) && is.null(x$specs$hab)) int <- x$specs$reg
        if (is.null(x$specs$reg) && !is.null(x$specs$hab)) int <- x$specs$hab
        if (!is.null(x$specs$reg) && !is.null(x$specs$hab))
            int <- interaction(x$specs$reg, x$specs$hab, drop=TRUE)
	nlev <- length(unique(int))        
	ssum <- numeric(n)}
    bray <- psum <- pfill <- vrow <- vcol <- numeric(n)
    for (i in 1:n) {
        bray[i] <- sum(abs(x$orig-x$perm[[i]]))/sum(x$orig+x$perm[[i]])
        psum[i] <- sum(x$orig) == sum(x$perm[[i]])
        pfill[i] <- sum(x$orig > 0) == sum(x$perm[[i]] > 0)
        vrow[i] <- sum(rowSums(x$orig) == rowSums(x$perm[[i]])) == nrow(x$orig)
        vcol[i] <- sum(colSums(x$orig) == colSums(x$perm[[i]])) == ncol(x$orig)
        if (restr) ssum[i] <- {sum(rowSums(aggregate(x$orig,list(int),sum)[,-1]) ==
            rowSums(aggregate(x$perm[[i]],list(int),sum)[,-1])) == nlev}
        }
    strsum <- if (restr) sum(ssum)/n else NA
    test <- c(sum=sum(psum)/n, fill=sum(pfill)/n, rowsums=sum(vrow)/n, colsums=sum(vcol)/n, strsum=strsum)
    x$perm <- NULL
    out <- list(x=x, bray=bray, test=test, restr=restr)
    class(out) <- c("summary.permat", "list")
    return(out)
}

## S3 print method for summary.permat
`print.summary.permat` <-
function(x, digits=2, ...)
{
    bray <- x$bray
    restr <- x$restr
    test <- x$test
    x <- x$x
    cat("Summary of object of class 'permat'\n\nCall: ")
    print(x$call)
    cat("Matrix type:", attr(x, "mtype"), "\nPermutation type:", attr(x, "ptype"))
    cat("\nRestricted:", restr, "\nFixed margins:", attr(x, "fixedmar"))
    cat("\n\nMatrix dimensions:", nrow(x$orig), "rows,", ncol(x$orig), "columns")
    cat("\nSum of original matrix:", sum(x$orig))
    cat("\nFill of original matrix:", round(sum(x$orig>0)/(nrow(x$orig)*ncol(x$orig)),digits))
    cat("\nNumber of permuted matrices:", attr(x, "times"),"\n")
    cat("\nMatrix sums retained:", round(100*test[1], digits), "%")
    cat("\nMatrix fill retained:", round(100*test[2], digits), "%")
    cat("\nRow sums retained:   ", round(100*test[3], digits), "%")
    cat("\nColumn sums retained:", round(100*test[4], digits), "%")
    if (restr) cat("\nSums within strata retained:", round(100*test[5], digits), "%")
    cat("\n\nBray-Curtis dissimilarities among original and permuted matrices:\n")
    print(summary(bray))
invisible(NULL)
}
