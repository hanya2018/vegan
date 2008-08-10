`treedive` <-
    function(comm, tree)
{
    if (!inherits(tree, "hclust"))
        stop("'clus' must be an 'hclust' result object")
    m <- as.matrix(cophenetic(tree))
    ## Repeat for sites
    div <- numeric(nrow(comm))
    for (i in 1:nrow(comm)) {
        k <- comm[i,] > 0
        d <- as.dist(m[k,k])
        cl <- update(tree, d = d)
        div[i] <- treeheight(cl)
    }
    div
}

