`var.null` <-
function(mat, no)
### Check for the presence of variables with null variances
{
    mat.cov <- cov(mat)
    problem <- FALSE
    for(i in 1:nrow(mat.cov)) {
        if(mat.cov[i,i] == 0) {
            cat("Matrix",no,"-- Variable no.",i," has a null variance",'\n')
            problem <- TRUE
        }
    }
    if(problem == TRUE) stop("Program stopped. Verify/modify your matrix No.",no)
}

