`ordixyplot` <-
    function(x, data = NULL, formula, display = "sites", choices=1:3,
             panel = "panel.ordi", ...)
{
    require(lattice) || stop
    x <- as.data.frame(scores(x, display = display, choices = choices))
    if (!is.null(data))
        x <- cbind(x, data)
    xyplot(formula, data = x, panel = panel,  ...)
}
