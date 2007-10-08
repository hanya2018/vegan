`var.null` <-
    function (mat, no) 
{
    problems <- diag(cov(mat)) <= 0
    if (any(problems)) {
        whichProbs <- paste(which(problems), collapse=", ")
        warning("zero variance in variable(s) ", whichProbs)
        stop("verify/modify your matrix No. ", no)
    }
    invisible(0)
}
