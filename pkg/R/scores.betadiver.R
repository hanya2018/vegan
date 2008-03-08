`scores.betadiver` <-
    function(x, ...)
{
    tot <- x$a + x$b + x$c
    a <- x$a/tot
    c <- x$c/tot
    y <- sqrt(0.75)*a
    x <- c + a/2
    cbind(x, y)
}

