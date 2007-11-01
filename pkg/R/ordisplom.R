`ordisplom` <-
    function(x, data=NULL, formula = NULL,  display = "sites", choices = 1:3,
             scaling = 2, panel = "panel.ordi", type = "p", ...)
{
    require(lattice) || stop("requires package 'lattice'")
    x <- as.data.frame(scores(x, display = display, choices = choices,
                              scaling = scaling))
    if (is.null(data))
        data <- x
    else if (is.null(formula))
        x <- cbind(x, data)
    ## type = "biplot" is not (yet?) implemented
    env <- list(arrows = NULL, centres = NULL)
    if (is.null(formula))
        pl <- splom(x, panel = panel, type = type, biplot = env, ...)
    else {
        formula <- as.formula(gsub("\\.", "x", deparse(formula)))
        pl <- splom(x = formula, data = data,  panel = panel, type = type,
                    bitplot = env, ...)
    }
    pl
}
