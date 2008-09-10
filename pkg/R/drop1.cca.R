`drop1.cca` <-
    function(object, scope, test = c("none", "permutation"),
             perm.max = 200, ...)
{
    test <- match.arg(test)
    out <- NextMethod("drop1", object, test="none", ...)
    cl <- class(out)
    if (test == "permutation") {
        adds <- anova(object, by = "margin", perm.max = perm.max, ...)
        nr <- nrow(adds)
        out <- cbind(out, rbind(NA, adds[-nr,3:5]))
        class(out) <- cl
    }
    out
}
