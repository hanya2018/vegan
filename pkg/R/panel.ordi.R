`panel.ordi` <-
    function(x, y, ...)
{
    panel.xyplot(x, y, ...)
    panel.abline(h=0, lty = 3, ...)
    panel.abline(v=0, lty = 3, ...)
}
