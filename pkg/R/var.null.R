`var.null` <-
    function (mat, no) 
{
    problems <- diag(cov(mat)) <= 0
    if (any(problems)) {
        warning("Zero variance in variable(s) ", which(problems))
        stop("Verify/modify your matrix No. ", no)
    }
    invisible(0)
}
