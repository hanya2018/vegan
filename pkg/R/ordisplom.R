`ordisplom` <-
    function(x, data=NULL, formula = NULL,  display = "sites", choices = 1:3,
             scaling = 2, panel = "panel.ordi", ...)
{
    require(lattice) || stop("requires package 'lattice'")
    x <- as.data.frame(scores(x, display = display, choices = choices,
                              scaling = scaling))
    if (is.null(data))
        data <- x
    else if (is.null(formula))
        x <- cbind(x, data)
    if (is.null(formula))
        pl <- splom(x, panel = panel, ...)
    else {
        formula <- as.formula(gsub("\\.", "x", deparse(formula)))
        pl <- splom(x = formula, data = data, panel = panel, ...)
    }
    pl
}
