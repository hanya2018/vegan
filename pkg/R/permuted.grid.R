`permuted.grid` <-
    function(nrow, ncol, mirror = FALSE, start.row = NULL, start.col = NULL)
{
    if(is.null(start.row))
        start.row <- .Internal(sample(nrow, 1, FALSE, NULL))
    if(is.null(start.col))
        start.col <- .Internal(sample(ncol, 1, FALSE, NULL))
    ir <- seq(start.row, length=nrow) %% nrow
    ic <- seq(start.col, length=ncol) %% ncol
    if (mirror) {
        if (runif(1) < 0.5)
            ir <- rev(ir)
        if (runif(1) < 0.5)
            ic <- rev(ic)
    }
    rep(ic, each=nrow) * nrow + rep(ir, len=nrow*ncol) + 1
}
