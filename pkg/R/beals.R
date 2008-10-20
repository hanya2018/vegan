
##############################################################
##  COMPUTES BEALS SMOOTHING FOR ALL SPECIES IN TABLE        #
##  This is a more complete function than the previous one   #
##  in the vegan package. The parameter values that give the #
##  equivalence are 'beals(x, x, mode=0,incSp=TRUE)'         #
##                                                           #
##  x     matrix to be replaced by beals values              #
##  refX  matrix to be used as source for joint occurrences  #
##  incSp flag to include target species in the computation  #
##  mode  sets the way to use abundance values               #
##       0 - presence/absence                                #
##       1 - abundances for conditioned probabilities        #
##       2 - abundances for weighted average                 #
##       3 - abundances for both                             #
##############################################################

beals <- function(x, refX = x, mode=0, incSp = TRUE)
{
    if(is.null(refX))
        refX <- x
    refX <- as.matrix(refX)
    x <- as.matrix(x)
    
    if(mode==0 || mode ==2) refX <- ifelse(refX > 0, 1, 0)
    if(mode==0 || mode ==1) x <- ifelse(x > 0, 1, 0)
    
    ## Computes conditioned probabilities
    M <- crossprod(ifelse(refX > 0, 1, 0), refX)
    C <- diag(M)
    M <- sweep(M, 2, replace(C,C==0,1), "/")
    if(!incSp) {
        for (i in 1:ncol(refX))
            M[i,i] <- 0
    }
    
    ## Average of conditioned probabilities
    S <- rowSums(x)
    b <-x
    for (i in 1:nrow(x)) {
        b[i, ] <- rowSums(sweep(M, 2, x[i, ], "*"))
    }
    
    SM <- rep(S,ncol(x))
    if(!incSp)
        SM <- SM - x
    b <- b/replace(SM,SM==0,1)
    b
}


##############################################################
## COMPUTES BEALS SMOOTHING FOR A GIVEN SPECIES              #
##                                                           #
##  x     matrix to be replaced by beals values              #
##  refX  matrix to be used as source for joint occurrences  #
##  incSp flag to include species in the computation         #
##  mode  sets the way to use abundance values               #
##       0 - presence/absence                                #
##       1 - abundances for conditioned probabilities        #
##       2 - abundances for weighted average                 #
##       3 - abundances for both                             #
##############################################################

beals.sp <- function(x, spIndex, refX = x, mode=0, incSp = FALSE)
{
    if(is.null(refX))
        refX <- x
    refX <- as.matrix(refX)
	
    if(mode==0 || mode ==2) refX <- ifelse(refX > 0, 1, 0)
    if(mode==0 || mode ==1) x <- ifelse(x > 0, 1, 0)
	
	
    ## Computes conditioned probabilities
    C <- colSums(refX)
    M <- crossprod(refX,ifelse(refX > 0, 1, 0)[,spIndex])
    M <- M/replace(C,C==0,1)
    if(!incSp)
        M[spIndex]<-0
	
    ## Average of conditioned probabilities
    b <- rowSums(sweep(x,2,M,"*"))
    S <- rowSums(x)
	
    if (!incSp)
        S <- S - x[,spIndex]
    b <- b/replace(S,S==0,1)
    b
}
