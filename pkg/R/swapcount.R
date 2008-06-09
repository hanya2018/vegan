`swapcount` <-
function(m, thin = 1)
{
## internal, is the 2x2 matrix diagonal or anti-diagonal
isDiag <- function(x) {
        x<- as.vector(x)
        x<- as.vector(x)
        X <- as.numeric(x>0)
        sX <- sum(X)
        choose <- c(min(x[c(2,3)]), min(x[c(1,4)]))
        if (sX == 4) {
            ch <- sample(c(1,2), 1)
            d <- choose[ch]
            if (ch == 2) ch <- -1
                return(d * ch)}
        if (identical(X, c(0,1,1,0)) | identical(X, c(0,1,1,1)) | identical(X, c(1,1,1,0)))
                return(choose[1])
        if (identical(X, c(1,0,0,1)) | identical(X, c(1,0,1,1)) | identical(X, c(1,1,0,1)))
                return(-choose[2])
        if (sX < 2 | identical(X, c(0,0,1,1)) | identical(X, c(1,1,0,0)) | 
            identical(X, c(0,1,0,1)) | identical(X, c(1,0,1,0)))
                return(0)
        }
    x <- as.matrix(m)
    n.col <- ncol(x)
    n.row <- nrow(x)
    changed <- 0
    while(changed < thin) {
        ran.row <- sample(n.row, 2)
        ran.col <- sample(n.col, 2)
        ev <- isDiag(x[ran.row, ran.col])
        if (ev != 0) {
            if (identical(sum(x[ran.row, ran.col] > 0), 
                sum(x[ran.row, ran.col] + matrix(c(ev,-ev,-ev,ev), 2, 2) > 0)))
                    x[ran.row, ran.col] <- x[ran.row, ran.col] + matrix(c(ev,-ev,-ev,ev), 2, 2)
            changed <- changed + 1}
        }
    return(x)
}
