`treedive` <-
    function(comm, tree, match.force = FALSE)
{
    if (!inherits(tree, "hclust"))
        stop("'clus' must be an 'hclust' result object")
    m <- as.matrix(cophenetic(tree))
    ## Check tree/comm match by names
    if (match.force || ncol(comm) != length(tree$order)) {
        fnd <- colnames(comm) %in% tree$labels
        if (!all(fnd)) {
            warning("not all names of 'comm' found in 'tree'")
            comm <- comm[, fnd]
        }
        fnd <- tree$labels %in% colnames(comm)
        if (!all(fnd))
            warning("not all names of 'tree' found in 'comm'")
        comm <- comm[, tree$labels[fnd]]
    }
    ## Repeat for sites
    div <- numeric(nrow(comm))
    for (i in 1:nrow(comm)) {
        k <- comm[i,] > 0
        d <- as.dist(m[k,k])
        cl <- update(tree, d = d)
        div[i] <- treeheight(cl)
    }
    names(div) <- rownames(comm)
    div
}

