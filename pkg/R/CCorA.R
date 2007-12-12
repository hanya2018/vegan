`CCorA` <-
    function(Y, X1, X2 = NULL, stand.Y = FALSE, stand.X1 = FALSE,
             stand.X2 = FALSE, nperm = 0, ...)
{
    require(MASS) || stop("requires packages 'MASS'")
    epsilon <- sqrt(.Machine$double.eps)
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
    temp <- cov.inv(Y.c, 1, epsilon)
    Y <- temp$mat
    pp <- temp$m
    rownames(Y) <- rownoms
    temp <- cov.inv(X1.c, 2, epsilon)
    X1 <- temp$mat
    if(partial) {            # Check and center X2
        X2 <- as.matrix(X2)
        var.null(X2,3)
        X2.c <- scale(X2, center = TRUE, scale = stand.X2)
        nX2 <- nrow(X2.c)
        if(nY != nX2) stop("Different numbers of rows in Y and X2")
        ## Replace X2.c by the table of its PCA object scores, computed by SVD
        temp2 <- cov.inv(X2.c,3, epsilon)
        X2 <- temp2$mat
        rr <- temp2$m
    }
    if(!partial) {
        X <- X1
        qq <- temp$m
    } else {
        ## Regress X1 on X2 and compute X = res(X1|X2) before canonical analysis
### Jari: remove next 2 lines and replace them by the next 6 lines
### Pierre: cannot see why this should be done. qr() handles 1-col matrices,
### and lm will do nothing else but add some heavy cruft over qr().
        Q <- qr(X2, tol=1e-6)
        X <- qr.resid(Q, X1)
### Pierre: Put back the old lines above and comment out six next:
        ## if(rr == 1) {
        ##    X <- as.matrix(residuals(lm(X1 ~ X2)))
        ##    } else {
        ##    Q <- qr(X2, tol=1e-6)
        ##    X <- qr.resid(Q, X1)
        ##    }
        ## Replace X by the table of its PCA object scores, computed by SVD
        temp <- cov.inv(X,4,epsilon)
        X <- temp$mat
        qq <- temp$m
    }
    rownames(X) <- rownoms
    ## Covariance matrices, etc. from the PCA scores
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
    XprX <- t(X) %*% X
    BB <- sqrt(n-1) * ginv(XprX) %*% t(X) %*% Cx
    ## Add row and column names
    rownames(AA) <- Ynoms
    if(!partial) {
        rownames(BB) <- Xnoms
    } else {
        res.rownames <- paste("XresAxis",1:nrow(BB),sep="")
        rownames(BB) <- res.rownames
    }
    rownames(Cy) <- rownames(Cx) <- rownoms
    colnames(AA) <- colnames(BB) <- colnames(Cy) <- colnames(Cx) <- axenames
    ## Compute the two redundancy statistics
    RsquareY.X <- simpleRDA2(Y, X)
    RsquareX.Y <- simpleRDA2(X, Y)
    Rsquare.adj.Y.X <- RsquareAdj(RsquareY.X$Rsquare, n, RsquareY.X$m)
    Rsquare.adj.X.Y <- RsquareAdj(RsquareX.Y$Rsquare, n, RsquareX.Y$m)
    ## Compute Pillai's trace = sum of the canonical eigenvalues
    ##                        = sum of the squared canonical correlations
    S11.inv <- S11.chol.inv %*% t(S11.chol.inv)
    S22.inv <- S22.chol.inv %*% t(S22.chol.inv)
    gross.mat <- S12 %*% S22.inv %*% t(S12) %*% S11.inv
    PillaiTrace <- sum(diag(gross.mat))
    s   <- min(pp, qq)
    df1 <- max(pp,qq)
    df2 <- (n - max(pp,qq) - 1)
    Fval  <- (PillaiTrace*df2)/((s-PillaiTrace)*df1)
    if(nperm > 0) {
        p.perm <- probPillai(Y, X, n, S11.inv, S22.inv, s, df1, df2,
                             epsilon, Fval, nperm, ...)
    } else {
        p.perm <- NA
    }
    if(!partial) {
        p.Pillai <- pf(Fval, s*df1, s*df2, lower.tail=FALSE)
    } else {
        p.Pillai <- NA
    }
    out <- list(Pillai=PillaiTrace, EigenValues=EigenValues, CanCorr=K.svd$d,
                Mat.ranks=c(RsquareX.Y$m, RsquareY.X$m), 
                RDA.Rsquares=c(RsquareY.X$Rsquare, RsquareX.Y$Rsquare),
                RDA.adj.Rsq=c(Rsquare.adj.Y.X, Rsquare.adj.X.Y),
                nperm=nperm, p.Pillai=p.Pillai, p.perm=p.perm,
                AA=AA, BB=BB, Cy=Cy, Cx=Cx, call = match.call())
    class(out) <- "CCorA"
    out
}

