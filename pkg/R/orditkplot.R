`orditkplot` <-
    function(x, display = "species", width, col = "black",
             bg="transparent", diam = 3, ...)
{
    require(tcltk) || stop("requires package tcltk")
    ## Graphical parameters and constants
    p <- par()
    ## PPI is points per inch, and p2p pixels per point
    PPI <- 72 
    p2p <- as.numeric(tclvalue(tcl("tk", "scaling"))) 
    ## Sanitize colours
    sanecol <- function(x) {
        if (is.na(x))
            x <- ""
        else if (is.numeric(x))
            x <- palette()[x]
        else if (x == "transparent")
            x <- ""
        x
    }
    p$bg <- sanecol(p$bg)
    p$fg <- sanecol(p$fg)
    p$col <- sanecol(p$col)
    p$col.axis <- sanecol(p$col.axis)
    p$col.lab <- sanecol(p$col.lab)
    pt.col <- sanecol(col)
    pt.bg <- sanecol(bg)
    ## toplevel
    w <- tktoplevel()
    tktitle(w) <- "orditkplot"
    ## Max dim of windows (depends on screen)
    YSCR <- as.numeric(tkwinfo("screenheight", w)) - 100
    XSCR <- as.numeric(tkwinfo("screenwidth", w))
    ## Buttons
    buts <- tkframe(w)
    tkpack(buts, side="bottom", fill="x", pady="2m")
    cp2eps <- tkbutton(buts, text="Copy to EPS", 
                       command=function() tkpostscript(can, x=0, y=0,
                       height=height, width=width, 
                       file=tkgetSaveFile(defaultextension=".eps")))
    dismiss <- tkbutton(buts, text="Dismiss", command=function() tkdestroy(w))

    ## Make canvas
    sco <- scores(x, display=display, ...)
    labs <- rownames(sco)
    ## Ranges and pretty values for axes
    xrange <- range(sco[,1])
    yrange <- range(sco[,2])
    xpretty <- pretty(xrange)
    ypretty <- pretty(yrange)
    ## Extend ranges by 4%
    tmp <- mean(xrange)
    xrange[1] <- 1.04*(xrange[1] - tmp) + tmp
    xrange[2] <- 1.04*(xrange[2] - tmp) + tmp
    xpretty <- xpretty[xpretty >= xrange[1] & xpretty <= xrange[2]]
    tmp <- mean(yrange)
    yrange[1] <- 1.04*(yrange[1] - tmp) + tmp
    yrange[2] <- 1.04*(yrange[2] - tmp) + tmp
    ypretty <- ypretty[ypretty >= yrange[1] & ypretty <= yrange[2]]
    ## Canvas width 6.99 inches, margins from par()
    if (missing(width))
        width <- 6.99
    width <- width * PPI * p2p
    mar <- round(p$mar * p$ps * p$cex * p2p)
    xusr <- width - mar[2] - mar[4]
    xincr <- xusr/diff(xrange)
    yincr <- xincr
    xy0 <- c(xrange[1], yrange[2]) # upper left corner
    ## Function to translate scores to canvas coordinates
    usr2xy <- function(row) {
        x <- round((row[1] - xy0[1]) * xincr) + mar[2]
        y <- round((xy0[2] - row[2]) * yincr) + mar[3]
        c(x,y)
    }
    ## Equal aspect ratio
    height <- round((diff(yrange)/diff(xrange)) * xusr)
    height <- height + mar[1] + mar[3]
    ## Canvas, finally
    can <- tkcanvas(w, relief="sunken", width=width, height=min(height,YSCR),
                    scrollregion=c(0,0,width,height))
    if (p$bg != "")
        tkconfigure(can, bg=p$bg)
    yscr <- tkscrollbar(w, command = function(...) tkyview(can, ...))
    tkconfigure(can, yscrollcommand = function(...) tkset(yscr, ...))
    ## Pack it up
    tkpack(can, side="left", fill="x")
    tkpack(yscr, side="right", fill="y")
    tkgrid(cp2eps, dismiss, sticky="s")

    ## Box
    x0 <- usr2xy(c(xrange[1], yrange[1]))
    x1 <- usr2xy(c(xrange[2], yrange[2]))
    tkcreate(can, "rectangle", x0[1], x0[2], x1[1], x1[2], outline = p$fg,
             width = p$lwd)
    ## Axes and ticks
    tl <- round(-p$tcl * p$ps)
    tmp <- xpretty
    for (i in 1:length(tmp)) {
        xx <- usr2xy(c(tmp[i], yrange[1]))
        tkcreate(can, "line", xx[1], xx[2], xx[1], xx[2]+tl, fill=p$fg)
        tkcreate(can, "text", xx[1], xx[2] + p$ps + max(tl, 0),
                 text=as.character(tmp[i]), fill=p$col.axis)
    }
    xx <- usr2xy(c(mean(xrange), yrange[1]))
    tkcreate(can, "text", xx[1], xx[2] + max(tl,0) + 2.5*p$ps,
             text=colnames(sco)[1], fill=p$col.lab)
    tmp <- ypretty
    for (i in 1:length(tmp)) {
        yy <- usr2xy(c(xrange[1], tmp[i]))
        tkcreate(can, "line", yy[1], yy[2], yy[1]-tl, yy[2], fill=p$fg )
        tkcreate(can, "text", yy[1]- max(tl, 0) - p$ps, yy[2],
                 text=as.character(tmp[i]), fill = p$col.axis)
    }
    ## Points and labels
    laboff <- round(p$ps/2 + diam + 1)
    for (i in 1:nrow(sco)) {
        xy <- usr2xy(sco[i,])
        item <- tkcreate(can, "oval", xy[1]-diam, xy[2]-diam,
                         xy[1]+diam,  xy[2]+diam, 
                         width=1, outline=pt.col, fill=pt.bg)
        lab <- tkcreate(can, "text", xy[1], xy[2]-laboff, text=labs[i],
                        fill = p$col)
        tkaddtag(can, "point", "withtag", item)
        tkaddtag(can, "label", "withtag", lab)
    }
    ## Plotting and Moving
    pDown <- function(x, y) {
        x <- as.numeric(x)
        y <- as.numeric(y)
        tkdtag(can, "selected")
        tkaddtag(can, "selected", "withtag", "current")
        tkitemraise(can, "current")
        .lastX <<- x
        .lastY <<- y
    }
    pMove <- function(x, y) {
        x <- as.numeric(x)
        y <- as.numeric(y)
        tkmove(can, "selected", x - .lastX, y - .lastY)
        .lastX <<- x
        .lastY <<- y
    }
    ## Dummy location of the mouse
    .lastX <- 0
    .lastY <- 0
    ## Highlight a label when mouse moves in
    tkitembind(can, "label", "<Any-Enter>",
               function() tkitemconfigure(can, "current", fill="red"))
    tkitembind(can, "label", "<Any-Leave>",
               function() tkitemconfigure(can, "current", fill="black"))
    tkitembind(can, "label", "<1>", pDown)
    tkitembind(can, "<ButtonRelease-1>", function(x) tkdtag(can, "selected")) 
    tkbind(can, "<B1-Motion>", pMove)
}

