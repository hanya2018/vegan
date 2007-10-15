`cov.inv` <-
function(mat, no, epsilon)
##
## This function returns:
##
## 1) mat = the matrix of PCA object scores (by SVD);
## 2) S.inv = the inverse of the covariance matrix;
## 3) m = the rank of matrix 'mat'
##
## The inverse of the PCA covariance matrix is simply the diagonal matrix of (1/eigenvalues).
## If ncol(mat) = 1, the inverse of the covariance matrix simply contains 1/var(mat).
{
    mat <- as.matrix(mat)
    if(ncol(mat) == 1) {
        S.inv <- as.matrix(1/var(mat))
        m <- 1
    } else {
        S.svd <- svd(cov(mat))
        m <- ncol(mat)
        mm <- length(which(S.svd$d > epsilon))
        if(mm < m) {
            if(no != 4) {
                message("Information - Matrix",no,": rank=",mm," < order",m)
            } else {
                message("Information - Matrix X = res(X1|X2): rank=",mm," < order",m)            
            }
            if((mm == 0) & (no == 4)) stop("X1 has rank = 0 after controlling for X2")
            m <- mm
        }
        S.inv <- diag(1/S.svd$d[1:m])
        mat <- mat %*% S.svd$u[,1:m]
    }
    list(mat=mat, S.inv=S.inv, m=m)
}
