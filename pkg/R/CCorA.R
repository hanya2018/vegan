### PL to JO, 07oct07
### In the listing, notes to you are indicated by ###
`CCorA` <-
    function(Y, X1, X2 = NULL, stand.Y = FALSE, stand.X1 = FALSE,
             stand.X2 = FALSE, nperm = 0, ...)
{
    require(MASS) || stop("requires packages 'MASS'")
    partial <- !is.null(X2)
    Y <- as.matrix(Y)
    var.null(Y,1)
    nY <- nrow(Y)
    p <- ncol(Y)
    Ynoms <- colnames(Y)
    X1 <- as.matrix(X1)
    var.null(X1,2)
    nX1 <- nrow(X1)
    q <- ncol(X1)
    Xnoms <- colnames(X1)
    if(nY != nX1) stop("Different numbers of rows in Y and X1")
    n <- nY
    if((p+q) >= (n-1)) stop("Not enough degrees of freedom!")
    rownoms <- rownames(Y)
    Y.c <- scale(Y, center = TRUE, scale = stand.Y)
    X1.c <- scale(X1, center = TRUE, scale = stand.X1)
    ## Replace Y.c and X1.c by tables of their PCA object scores, computed by SVD
    temp <- cov.inv(Y.c,1)
    Y <- temp$mat
    S.Y.inv <- temp$S.inv
    rownames(Y) <- rownoms
    temp <- cov.inv(X1.c,2)
    X1 <- temp$mat
    S.X1.inv <- temp$S.inv
    if(partial) {            # Compute residuals of X1 over X2
        X2 <- as.matrix(X2)
        var.null(X2,3)
        X2.c <- scale(X2, center = TRUE, scale = stand.X2)
        nX2 <- nrow(X2.c)
        if(nY != nX2) stop("Different numbers of rows in Y and X2")
        ## Replace X2.c by the table of its PCA object scores, computed by SVD
        temp2 <- cov.inv(X2.c,3)
        X2 <- temp2$mat
    }
    if(!partial) {
        X <- X1
        S.X.inv <- S.X1.inv
    } else {
        ## Regress X1 on X2 and compute res(X1)/X2 before the canonical analysis to estimate [a]
        Q <- qr(X2, tol=1e-6)
        X <- qr.resid(Q, X1)
        ## Replace X by the table of its PCA object scores, computed by SVD
        temp <- cov.inv(X,4)
        X <- temp$mat
        S.X.inv <- temp$S.inv
    }
    rownames(X) <- rownoms
    ## Covariance matrices, etc. from the PCA scores
    epsilon <- sqrt(.Machine$double.eps)
    S11 <- cov(Y)
    if(sum(abs(S11)) < epsilon) return(0)
    S22 <- cov(X)
    if(sum(abs(S22)) < epsilon) return(0)
    S12 <- cov(Y,X)
    if(sum(abs(S12)) < epsilon) return(0)
    S11.chol <- chol(S11)
    S11.chol.inv <- solve(S11.chol)
    S22.chol <- chol(S22)
    S22.chol.inv <- solve(S22.chol)
    ## K summarizes the correlation structure between the two sets of variables
    K <- t(S11.chol.inv) %*% S12 %*% S22.chol.inv
    K.svd <- svd(K)
    EigenValues <- K.svd$d^2
    ## K.svd$u %*% diag(K.svd$d) %*% t(K.svd$v)   # Check that K = U D V'
    axenames <- paste("CanAxis",1:length(K.svd$d),sep="")
    U <- K.svd$u
    V <- K.svd$v
    A <- S11.chol.inv %*% U
    B <- S22.chol.inv %*% V
    Cy <- (Y %*% A)/sqrt(n-1)
    Cx <- (X %*% B)/sqrt(n-1)
    ## Compute the 'Biplot scores of Y variables' a posteriori --
    ## use 'ginv' for inversion in case there is collinearity
    ## AA <- coefficients of the regression of Cy on Y.c, times sqrt(n-1)
    ## AA <- sqrt(n-1) * [Y'Y]-1 Y' Cy
    YprY <- t(Y.c) %*% Y.c
    AA <- sqrt(n-1) * ginv(YprY) %*% t(Y.c) %*% Cy
    ##
    ## Compute the 'Biplot scores of X variables' a posteriori --
    XprX <- t(X1.c) %*% X1.c
    BB <- sqrt(n-1) * ginv(XprX) %*% t(X1.c) %*% Cx
    ## Add row and column names
    rownames(U) <- rownames(AA) <- Ynoms
    rownames(V) <- rownames(BB) <- Xnoms
    rownames(Cy) <- rownames(Cx) <- rownoms
    colnames(U) <- colnames(A) <- colnames(AA) <- colnames(V) <- colnames(B) <- colnames(BB) <- colnames(Cy) <- colnames(Cx) <- axenames
    ## Compute the two redundancy statistics
    RsquareY.X <- simpleRDA2(Y, X)
    RsquareX.Y <- simpleRDA2(X, Y)
    Rsquare.adj.Y.X <- RsquareAdj(RsquareY.X$Rsquare, n, RsquareY.X$m)
    Rsquare.adj.X.Y <- RsquareAdj(RsquareX.Y$Rsquare, n, RsquareX.Y$m)
    ## Compute Pillai's trace = sum of the canonical eigenvalues
    ##                        = sum of the squared canonical correlations
    ## Next 3 lines have been modified
    S11.inv = solve(S11)
    S22.inv = solve(S22)
    gross.mat <- S12 %*% S22.inv %*% t(S12) %*% S11.inv
    PillaiTrace <- sum(diag(gross.mat))
### New lines: compute the parametric prob associated with Pillai's trace ...
### 'cat' prints are including only for testing the changes. 
### Remove them when 'p.Pillai', 'nperm' and 'p.perm' have been incorporated to
### `print.CCorA`
    n1 <- abs(RsquareX.Y$m - RsquareY.X$m) - 1
    n2 <- n - RsquareX.Y$m - RsquareY.X$m - 2
    s  <- min(RsquareX.Y$m, RsquareY.X$m)
    df1<- n1 + s + 1
    df2<- n2 + s + 1
    F  <- (PillaiTrace*df2)/((s-PillaiTrace)*df1)
    p.Pillai <- pf(F,s*df1,s*df2,lower.tail=FALSE)
    cat("PillaiTrace =",PillaiTrace,"  n =",n,"  F =",F,"  p.Pillai =",p.Pillai,'\n')
    if(nperm > 0) {
       p.perm <- probPillai(Y,X,n,S11.inv,S22.inv,s,df1,df2,epsilon,F,nperm)
       cat('F =',F,'  Prob(',nperm,'permutations) =',p.perm,'\n')
       }
### ... till here.
    out <- list(Pillai=PillaiTrace, EigenValues=EigenValues, CanCorr=K.svd$d,
                Mat.ranks=c(RsquareX.Y$m, RsquareY.X$m), 
                RDA.Rsquares=c(RsquareY.X$Rsquare, RsquareX.Y$Rsquare),
                RDA.adj.Rsq=c(Rsquare.adj.Y.X, Rsquare.adj.X.Y),
                AA=AA, BB=BB, Cy=Cy, Cx=Cx, call = match.call())
    class(out) <- "CCorA"
    out
}
