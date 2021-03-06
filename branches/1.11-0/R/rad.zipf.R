"rad.zipf" <-
    function (x, family = poisson, ...) 
{
    x <- as.rad(x)
    rnk <- seq(along = x)
    off <- rep(log(sum(x)), length(x))
    fam <- family(link = "log")
    ln <- try(glm(x ~ log(rnk) + offset(off), family = fam))
    if (inherits(ln, "try-error")) {
        aic <- rdf <- ln <- nl <- dev <- NA
        p <- rep(NA, 2)
        fit <- res <- wts <- rep(NA, length(x))
    }
    else {
        fit <- fitted(ln)
        p <- coef(ln)
        p[1] <- exp(p[1])
        aic <- AIC(ln)
        rdf <- df.residual(ln)
        dev <- deviance(ln)
        res <- ln$residuals
        wts <- weights(ln)
    }
    names(p) <- c("p1", "gamma")
    out <- list(model = "Zipf", family = fam, y = x, coefficients = p, 
                fitted.values = fit, aic = aic, rank = 2, df.residual = rdf, 
                deviance = dev, residuals = res, prior.weights = wts)
    class(out) <- c("radline", "glm")
    out
}
