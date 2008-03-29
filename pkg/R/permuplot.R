`permuplot` <- function(n, control = permControl(),
                        col = par("col"),
                        hcol = "red",
                        xlim=NULL, ylim=NULL,
                        inset = 0.1,
                        main=NULL, sub=NULL,
                        ann = par("ann"),
                        ...) {
    xy.series <- function(n) {
        angle <- seq(0, 2*pi, length = n+1)[-(n+1)]
        x <- rev(cos(angle))
        y <- rev(sin(angle))
        xy <- xy.coords(x, y)
        return(xy)
    }
    xy.free <- function(n) {
        x <- runif(n)
        y <- runif(n)
        xy <- xy.coords(x, y)
        return(xy)
    }
    xy.grid <- function(ncol, nrow) {
        x <- rep(seq_len(ncol), each = nrow)
        y <- rev(rep(seq_len(nrow), times = ncol))
        xy <- xy.coords(x, y)
        return(xy)
    }
    axis.limits <- function(vals, inset) {
        lim <- range(vals[is.finite(vals)])
        lim.range <- lim[2] - lim[1]
        res <- c(lim[1] - (lim.range * inset),
                 lim[2] + (lim.range * inset))
        return(res)
    }
    use.strata <- !is.null(control$strata)
    if(use.strata) {
        tab <- table(control$strata)
        n.grp <- length(tab)
        opar <- par(no.readonly=TRUE, mar=c(2,2,2,1)+0.1,
                    mfrow = n2mfrow(n.grp),
                    oma=c(2.1,0,3.1,0))
        on.exit(par(opar))
        xy <- switch(control$type,
                     free = lapply(tab, xy.free),
                     series = lapply(tab, xy.series),
                     grid = lapply(tab, function(x) {
                         xy.grid(control$ncol, control$nrow)
                     }),
                     stop("Unsupport permutation 'type'"))
        perms <- permuted.index2(n, control = control)
        perms <- tapply(perms, control$strata, function(x) x)
        if(is.null(main))
            main <- paste("Stratum:", names(tab))
        for(i in seq_along(xy)) {
            if(is.null(xlim))
                xlim <- axis.limits(xy[[i]]$x, inset)
            if(is.null(ylim))
                ylim <- axis.limits(xy[[i]]$y, inset)
            plot.new()
            plot.window(xlim, ylim, asp = 1, ...)
            cols <- switch(control$type,
                           free = rep(col, tab[i]),
                           series = c(hcol, rep(col, tab[i]-1)),
                           grid = {cols <- rep(col, tab[i])
                                   cols[which.min(perms[[i]])] <-
                                       hcol
                                   cols})
            text(xy[[i]]$x, xy[[i]]$y, labels = perms[[i]],
                 col = cols, ...)
            if(ann) {
                title(main = main[i],  ...)
                title(sub = paste("n in stratum:", tab[i]),
                      line = 0.5, ...)
            }
            box()
        }
        if(ann) {
            sub <- paste(paste("n: ", n, ";", sep = ""),
                         paste("mirror: ", control$mirror, ";",
                               sep = ""),
                         paste("constant: ", control$constant, ";",
                               sep = ""),
                         sep = "    ")
            if(control$type == "grid")
                sub <- paste(sub, paste("ncol: ",
                                        control$ncol, ";",
                                        sep = ""),
                             paste("nrow: ", control$nrow, ";",
                                   sep = ""),
                             sep = "    ")
            title(main = paste("Permutation type:", control$type),
                  outer = TRUE, cex.main = 1.75, ...)
            title(sub = sub, outer = TRUE, line = 0.5,
                  cex.sub = 1, ...)
        }
    } else {
        xy <- switch(control$type,
                     free = xy.free(n),
                     series = xy.series(n),
                     grid = xy.grid(control$ncol, control$nrow),
                     stop("Unsupport permutation 'type'"))
        if(is.null(xlim)) {
            xlim <- axis.limits(xy$x, inset)
        }
        if(is.null(ylim)) {
            ylim <- axis.limits(xy$y, inset)
        }
        opar <- par(no.readonly=TRUE, mar=c(2,1,3,1)+0.1)
        on.exit(par(opar))
        if(is.null(main))
            main <- paste("Permutation type:", control$type)
        if(is.null(sub))
            sub <- paste(paste("n: ", n, ";", sep = ""),
                         paste("mirror: ", control$mirror, ";",
                               sep = ""),
                         sep = "      ")
        plot.new()
        plot.window(xlim, ylim, asp = 1, ...)
        labs <- permuted.index2(n, control=control)
        cols <- switch(control$type,
                       free = rep(col, n),
                       series = c(hcol, rep(col, n-1)),
                       grid = {cols <- rep(col, n)
                               cols[which.min(labs)] <- hcol
                               cols})
        text(xy$x, xy$y, labels = labs,
             col = cols, ...)
        if(ann) {
            title(main = main, ...)
            title(sub = sub, line = 0.5, ...)
        }
        box()
    }
    invisible()
}
