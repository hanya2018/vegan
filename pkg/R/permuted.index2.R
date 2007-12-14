`permuted.index2` <-
    function (n, control = permControl())
{
    if (is.null(control$strata)) {
        out <- switch(control$type,
                      "free" = .Internal(sample(n, n, FALSE, NULL)),
                      "series" = permuted.series(1:n, mirror = control$mirror),
                      "grid" = permuted.grid(nrow = control$nrow,
                      ncol = control$ncol, mirror = control$mirror)
                      )
    } else {
        out <- 1:n
        inds <- names(table(control$strata))
        if(control$constant) {
            if(control$type == "series") {
                start <- .Internal(sample(n / length(inds), 1, FALSE, NULL))
            } else if(control$type == "grid") {
                start.row <- .Internal(sample(control$nrow, 1, FALSE, NULL))
                start.col <- .Internal(sample(control$ncol, 1, FALSE, NULL))
            }
            for (is in inds) {
                gr <- out[control$strata == is]
                if ((n.gr <- length(gr))> 1) {
                    out [gr]<- switch(control$type,
                                      "free" = .Internal(sample(n.gr, n.gr,
                                      FALSE, NULL)),
                                      "series" = permuted.series(gr,
                                      mirror = control$mirror, start = start),
                                      "grid" = permuted.grid(nrow = control$nrow,
                                      ncol = control$ncol,
                                      mirror = control$mirror,
                                      start.row = start.row,
                                      start.col = start.col)
                                      )
                }
            }
        } else {
            for (is in inds) {
                gr <- out[control$strata == is]
                if ((n.gr <- length(gr))> 1) {
                    out [gr]<- switch(control$type,
                                      "free" = .Internal(sample(n.gr, n.gr,
                                      FALSE, NULL)),
                                      "series" = permuted.series(gr,
                                      mirror = control$mirror),
                                      "grid" = permuted.grid(nrow = control$nrow,
                                      ncol = control$ncol,
                                      mirror = control$mirror)
                                      )
                }
            }
        }
    }
    out
}
