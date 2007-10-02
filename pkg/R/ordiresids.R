`ordiresids` <-
    function(x, kind = c("residuals", "scale", "qqnorm"), residuals = "working",
             type = c("p", "smooth", "g"), formula, ...)
{
    require(lattice) || stop("requires package lattice")
    kind <- match.arg(kind)
    if (!inherits(x, "cca"))
        stop("function is only available for constrained ordination")
    fit <- fitted(x, type = residuals)
    res <- residuals(x, type = residuals)
    colnam <- rep(colnames(fit), each=nrow(fit))
    rownam <- rep(rownames(fit), ncol(fit))
    df <- data.frame(Fitted = as.vector(fit), Residuals = as.vector(res),
                     Sites=rownam, Species = colnam)
    if (kind == "residuals") {
        if (missing(formula))
            formula <- as.formula(Residuals ~ Fitted)
        pl <- xyplot(formula, data = df, type = type, ...)
    }
    if (kind == "scale") {
        if (missing(formula))
            formula <- as.formula(sqrt(abs(Residuals)) ~ Fitted)
        pl <- xyplot(formula, data = df, type = type, ...)
    }
    if (kind == "qqnorm") {
        if (missing(formula))
            formula <- as.formula(~ Residuals)
        pl <- qqmath(formula, data = df, type = type, ...)
    }
    pl
}
