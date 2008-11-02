## permatswap function
`permatswap` <-
function(m, method="quasiswap", reg=NULL, hab=NULL, mtype="count", times=100, burnin = 10000, thin = 1000)
{
## temporary internal function
## quasiswapcount is based on the idea of Carsten Dorman from function swap.web::bipartite
quasiswapcount <-
function(m)
{

## internal fun
incRease <- function(x, increase)
{
    x<- as.vector(x)
    X <- as.numeric(x>0)
    ## sX: number of non-zero cells
    sX <- sum(X)
    ## Smallest antidiagonal(1) and diagonal(2)  element
    choose <- c(min(x[c(2,3)]), min(x[c(1,4)]))
    ## if fill should be increased
    if (increase) {
        if (identical(X, c(0,1,1,0)) || identical(X, c(0,1,1,1)) || identical(X, c(1,1,1,0))) {
            if (choose[1] > 1)
                ## this step to modify choose[] is needed to increase fill
                return(choose[1] - sample(choose[1]-1, 1))
            else return(0)
        } else {
        if (identical(X, c(1,0,0,1)) || identical(X, c(1,0,1,1)) || identical(X, c(1,1,0,1))) {
            if (choose[2] > 1)
                return(-(choose[2] - sample(choose[2]-1, 1)))
            else return(0)
        } else return(0)
        }
    ## if fill should be decreased
    } else {

        if (identical(X, c(1,1,1,1)) || identical(X, c(0,1,1,1)) || identical(X, c(1,1,1,0))) {
            return(choose[1])
        } else {
        if (identical(X, c(1,0,1,1)) || identical(X, c(1,1,0,1)))
            return(-choose[2])
        else return(0)
        }
    }
} ## end of internal fun

    x <- as.matrix(m)
    n.col <- ncol(x)
    n.row <- nrow(x)
    fill.orig <- sum(x > 0)
    ## marginal totals are retained, but not matrix fill
    x <- r2dtable(1, apply(x, 1, sum), apply(x, 2, sum))[[1]]
    ## if fill is o.k., no need for further steps (this is rarely the case)
    if (sum(x > 0) == fill.orig)
        return(x)
    ## if fill is not o.k.
    else {
    ## while loop as long as fill is o.k.
        fill.changed <- sum(x > 0)
        while(fill.changed != fill.orig) {
            rr <- sample(n.row, 2)
            rc <- sample(n.col, 2)
            ev <- incRease(x[rr, rc], increase = fill.changed < fill.orig)
            if (ev != 0)
                x[rr, rc] <- x[rr, rc] + matrix(c(ev,-ev,-ev,ev), 2, 2)
            fill.changed <- sum(x > 0)
        }
        return(x)
    }
}



    if (!identical(all.equal(m, round(m)), TRUE))
       stop("function accepts only integers (counts)")
    mtype <- match.arg(mtype, c("prab", "count"))
    count <- mtype == "count"
    if (count) {
        method <- match.arg(method, c("swap", "quasiswap"))
    } else {method <- match.arg(method, c("swap", "quasiswap", "tswap"))}

    m <- as.matrix(m)
    n.row <- nrow(m)
    n.col <- ncol(m)
    if (mtype == "prab") m <- ifelse(m > 0, 1, 0)
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
        if (method != "quasiswap") {
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
            } # for i end
        } else {
            for (i in 1:times) {
                if (count)
                    perm[[i]][id,] <- quasiswapcount(temp)                      ## this should be replaced by .C()
                else perm[[i]][id,] <- commsimulator(temp, method=method)
            }
            thin <- burnin <- 0
        }
    } # for j end
    specs <- list(reg=reg, hab=hab, burnin=burnin, thin=thin)
    out <- list(call=match.call(), orig=m, perm=perm, specs=specs)
    attr(out, "mtype") <- mtype
    attr(out, "ptype") <- "swap"
    attr(out, "fixedmar") <- "both"
    attr(out, "times") <- times
    attr(out, "shuffle") <- NA
    class(out) <- c("permat", "list")
    return(out)
}


