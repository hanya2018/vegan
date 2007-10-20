`ordixyplot` <-
    function(x, data = NULL, formula, display = "sites", choices=1:3,
             scaling = 2, panel = "panel.ordi", aspect = "iso",  ...)
{
    require(lattice) || stop("requires package 'lattice'")
    x <- as.data.frame(scores(x, display = display, choices = choices,
                              scaling = scaling))
    if (!is.null(data))
        x <- cbind(x, data)
    if (missing(formula)) {
        v <- colnames(x)
        formula <- as.formula(paste(v[2], "~", v[1]))
    }
    xyplot(formula, data = x, panel = panel,  aspect = aspect, ...)
}
