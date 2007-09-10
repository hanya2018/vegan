`var.null` <-
    function (mat, no) 
{
    vars <- diag(cov(mat))
    if (any(vars <= 0))
        stop("Verify/modify your matrix No.", no)
}
