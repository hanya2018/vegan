`ordicloud` <-
    function(x, data = NULL, formula, display = "sites", choices=1:3,
             panel = "panel.cloud", ...)
{
    require(lattice) || stop("requires package 'lattice'")
    x <- as.data.frame(scores(x, display = display, choices = choices))
    if (!is.null(data))
        x <- cbind(x, data)
    cloud(formula, data = x, panel = panel, ...)
}
