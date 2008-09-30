`head.cca` <-
    function(x, n=6, tail = 0, ...) {
        print(summary(x, ...), head=n, tail=tail)
    }

`tail.cca` <-
    function(x, n=6, head = 0, ...) {
        print(summary(x, ...), head=head, tail=n)
    }
