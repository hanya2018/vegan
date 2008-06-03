`print.mso` <-
    function(x,  digits = max(3, getOption("digits") - 3), ...)
{
    NextMethod(x, "print", ...)
    cat("mso variogram:\n\n")
    print(x$vario, digits = digits, ...)
    invisible(x)
}

