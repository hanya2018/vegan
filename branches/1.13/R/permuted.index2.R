`permuted.index2` <-
    function (n, control = permControl())
{
    `permuted.strata` <-
        function(strata)
        {
            lev <- length(levels(strata))
            ngr <- length(strata) / lev
            sp <- split(seq(along = strata), strata)
            unname(do.call(c, sp[.Internal(sample(lev, lev, FALSE, NULL))]))
        }
    `permuted.grid` <-
        function(nrow, ncol, mirror = FALSE,
                 start.row = NULL, start.col = NULL,
                 flip = NULL)
        {
            if(is.null(start.row))
                start.row <- .Internal(sample(nrow, 1, FALSE, NULL))
            if(is.null(start.col))
                start.col <- .Internal(sample(ncol, 1, FALSE, NULL))
            ir <- seq(start.row, length=nrow) %% nrow
            ic <- seq(start.col, length=ncol) %% ncol
            if(!is.null(flip)) {
                if(any(flip)) {
                    if(flip[1])
                        ir <- rev(ir)
                    if(flip[2])
                        ic <- rev(ic)
                }
            } else {
                if (mirror) {
                    if (runif(1) < 0.5)
                        ir <- rev(ir)
                    if (runif(1) < 0.5)
                        ic <- rev(ic)
                }
            }
            rep(ic, each=nrow) * nrow + rep(ir, len=nrow*ncol) + 1
        }
    `permuted.series` <- function(inds, mirror = FALSE,
                                  start = NULL, flip=NULL)
    {
        n <- length(inds)
        if(is.null(start))
            start <- .Internal(sample(n, 1, FALSE, NULL))
        out <- seq(start, length = n) %% n + 1
        if(!is.null(flip)) {
            if(flip)
                out <- rev(out)
        } else {
            if(mirror && runif(1) < 0.5)
                out <- rev(out)
        }
        inds[out]
    }
    if (is.null(control$strata)) {
        out <- switch(control$type,
                      "free" = .Internal(sample(n, n, FALSE, NULL)),
                      "series" = permuted.series(1:n, mirror = control$mirror),
                      "grid" = permuted.grid(nrow = control$nrow,
                      ncol = control$ncol, mirror = control$mirror)
                      )
    } else if(control$type == "strata") {
        out <- permuted.strata(control$strata)
    } else {
        out <- 1:n
        inds <- names(table(control$strata))
        if(control$constant) {
            if(control$type == "series") {
                start <- .Internal(sample(n / length(inds), 1, FALSE, NULL))
                flip <- runif(1) < 0.5
            } else if(control$type == "grid") {
                start.row <- .Internal(sample(control$nrow, 1, FALSE, NULL))
                start.col <- .Internal(sample(control$ncol, 1, FALSE, NULL))
                flip <- runif(2) < 0.5
            }
        } else {
            start <- start.row <- start.col <- flip <- NULL
        }
        for (is in inds) {
            gr <- out[control$strata == is]
            if ((n.gr <- length(gr))> 1) {
                out[gr] <- switch(control$type,
                                  "free" = out[gr][.Internal(sample(n.gr, n.gr,
                                  FALSE, NULL))],
                                  "series" = permuted.series(gr,
                                  mirror = control$mirror, start = start,
                                  flip = flip),
                                  "grid" = gr[permuted.grid(nrow = control$nrow,
                                  ncol = control$ncol,
                                  mirror = control$mirror,
                                  start.row = start.row,
                                  start.col = start.col,
                                  flip = flip)]
                                  )
            }
        }
    }
    out
}
