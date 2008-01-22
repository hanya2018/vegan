`orditkplot` <-
    function(x, display = "species", cex=0.8, width, col = "black",
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
    ## Define fonts
    idx <- match(p$family, c("","serif","sans","mono"))
    if (!is.na(idx))
        p$family <- c("helvetica", "times", "helvetica", "courier")[idx]
    saneslant <- function(x) {
        list("roman", "bold", "italic", c("bold", "italic"))[[x]]
    }
    fnt <- c(p$family, round(p$ps*p$cex*cex), saneslant(p$font))
    fnt.axis <- c(p$family, round(p$ps*p$cex.axis), saneslant(p$font.axis))
    fnt.lab <- c(p$family, round(p$ps*p$cex.lab), saneslant(p$font.lab))
    ## toplevel
    w <- tktoplevel()
    tktitle(w) <- "orditkplot"
    ## Max dim of windows (depends on screen)
    YSCR <- as.numeric(tkwinfo("screenheight", w)) - 100
    XSCR <- as.numeric(tkwinfo("screenwidth", w))
    ## Buttons
    buts <- tkframe(w)
    cp2eps <- tkbutton(buts, text="Copy to EPS", 
                       command=function() tkpostscript(can, x=0, y=0,
                       height=height, width=width, 
                       file=tkgetSaveFile(defaultextension=".eps")))
    dismiss <- tkbutton(buts, text="Dismiss", command=function() tkdestroy(w))
    ## Button to dump new label locations to an R object
    pDump <- function() {
        xy <- matrix(0, nrow=nrow(sco), ncol=2)
        rownames(xy) <- rownames(sco)
        colnames(xy) <- colnames(sco)
        for(nm in names(pola)) {
            xy[tclvalue(labtext[[nm]]),] <- xy2usr(nm)
        }
        dumpVar <- tclVar("")
        tt <- tktoplevel()
        tktitle(tt) <- "R Dump"
        entryDump <- tkentry(tt, width=20, textvariable=dumpVar)
        tkgrid(tklabel(tt, text="Enter name for an R object"))
        tkgrid(entryDump)
        isDone <- function() {
            dumpName <- tclvalue(dumpVar)
            if (exists(dumpName, envir=.GlobalEnv)) {
                ok <- tkmessageBox(message=paste(dumpName, "exists.\nOK to overwrite?"),
                                   icon="warning", type="okcancel", default="ok")
                if(tclvalue(ok) == "ok") {
                    assign(dumpName, xy, envir=.GlobalEnv)
                    tkdestroy(tt)
                }
            }
            else {
                assign(dumpName, xy, envir=.GlobalEnv)
                tkdestroy(tt)
            }
        }
        tkbind(entryDump, "<Return>", isDone)
        tkfocus(tt)
    }
    dump <- tkbutton(buts, text="Dump to R", command=pDump)
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
    ## Canvas like they were in the default devices when I last checked
    if (missing(width)) {
        if (options("device") == "quartz")
            width <- 5
        else
            width <- 6.99
    }
    width <- width * PPI * p2p
    ## Margin row width also varies with platform and devices
    ## rpix <- (p$mai/p$mar * PPI * p2p)[1]
    rpix <- p$cra[2]
    mar <- round(p$mar * rpix)
    xusr <- width - mar[2] - mar[4]
    xincr <- xusr/diff(xrange)
    yincr <- xincr
    xy0 <- c(xrange[1], yrange[2]) # upper left corner
    ## Functions to translate scores to canvas coordinates and back
    usr2xy <- function(row) {
        x <- round((row[1] - xy0[1]) * xincr) + mar[2]
        y <- round((xy0[2] - row[2]) * yincr) + mar[3]
        c(x,y)
    }
    xy2usr <- function(item) {
        xy <- as.numeric(tkcoords(can, item))
        x <- xy[1] 
        y <- xy[2] 
        x <- xrange[1] + (x - mar[2])/xincr 
        y <- yrange[2] - (y - mar[3])/yincr 
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
    tkpack(buts, side="bottom", fill="x", pady="2m")
    tkpack(can, side="left", fill="x")
    tkpack(yscr, side="right", fill="y")
    tkgrid(cp2eps, dump, dismiss, sticky="s")
    
    ## Box
    x0 <- usr2xy(c(xrange[1], yrange[1]))
    x1 <- usr2xy(c(xrange[2], yrange[2]))
    tkcreate(can, "rectangle", x0[1], x0[2], x1[1], x1[2], outline = p$fg,
             width = p$lwd)
    ## Axes and ticks
    tl <- -p$tcl * p$ps * p2p
    axoff <- p$mgp[3] * rpix
    tmp <- xpretty
    for (i in 1:length(tmp)) {
        x0 <- usr2xy(c(xpretty[1], yrange[1]))
        x1 <- usr2xy(c(xpretty[length(xpretty)], yrange[1]))
        tkcreate(can, "line", x0[1], x0[2]+axoff, x1[1], x1[2]+axoff,
                 fill=p$fg)
        xx <- usr2xy(c(tmp[i], yrange[1]))
        tkcreate(can, "line", xx[1], xx[2] + axoff, xx[1], xx[2]+tl+axoff,
                 fill=p$fg)
        tkcreate(can, "text", xx[1], xx[2] + rpix * p$mgp[2], anchor="n",
                 text=as.character(tmp[i]), fill=p$col.axis, font=fnt.axis)
    }
    xx <- usr2xy(c(mean(xrange), yrange[1]))
    tkcreate(can, "text", xx[1], xx[2] + rpix * p$mgp[1],
             text=colnames(sco)[1], fill=p$col.lab, anchor="n", font=fnt.lab)
    tmp <- ypretty
    for (i in 1:length(tmp)) {
        x0 <- usr2xy(c(xrange[1], tmp[1]))
        x1 <- usr2xy(c(xrange[1], tmp[length(tmp)]))
        tkcreate(can, "line", x0[1]-axoff, x0[2], x1[1]-axoff, x1[2])
        yy <- usr2xy(c(xrange[1], tmp[i]))
        tkcreate(can, "line", yy[1]-axoff, yy[2], yy[1]-tl-axoff, yy[2],
                 fill=p$fg )
        tkcreate(can, "text", yy[1] - rpix * p$mgp[2] , yy[2], anchor="e",
                 text=as.character(tmp[i]), fill = p$col.axis, font=fnt.axis)
    }
    ## Points and labels
    laboff <- round(p$ps/2 + diam + 1)
    pola <- tclArray()
    labtext <- tclArray()
    for (i in 1:nrow(sco)) {
        xy <- usr2xy(sco[i,])
        item <- tkcreate(can, "oval", xy[1]-diam, xy[2]-diam,
                         xy[1]+diam,  xy[2]+diam, 
                         width=1, outline=pt.col, fill=pt.bg)
        lab <- tkcreate(can, "text", xy[1], xy[2]-laboff, text=labs[i],
                        fill = p$col, font=fnt)
        tkaddtag(can, "point", "withtag", item)
        tkaddtag(can, "label", "withtag", lab)
        pola[[lab]] <- item
        labtext[[lab]] <- labs[i]
    }
    ## Plotting and Moving
    pDown <- function(x, y) {
        x <- as.numeric(x)
        y <- as.numeric(y)
        tkdtag(can, "selected")
        tkaddtag(can, "selected", "withtag", "current")
        tkitemraise(can, "current")
        p <- as.numeric(tkcoords(can,
                                 pola[[tkfind(can, "withtag", "current")]]))
        .pX <<- (p[1]+p[3])/2
        .pY <<- (p[2]+p[4])/2
        .lastX <<- x
        .lastY <<- y
    }
    pMove <- function(x, y) {
        x <- as.numeric(x)
        y <- as.numeric(y)
        tkmove(can, "selected", x - .lastX, y - .lastY)
        tkdelete(can, "ptr")
        .lastX <<- x
        .lastY <<- y
        xadj <- as.numeric(tkcanvasx(can, 0))
        yadj <- as.numeric(tkcanvasy(can, 0))
        conn <- tkcreate(can, "line", .lastX + xadj, .lastY+yadj,
                         .pX, .pY, fill="red")
        tkaddtag(can, "ptr", "withtag", conn)
    }
    ## Dummy location of the mouse
    .lastX <- 0
    .lastY <- 0
    .pX <- 0
    .pY <- 0
    ## Highlight a label when mouse moves in
    tkitembind(can, "label", "<Any-Enter>",
               function() tkitemconfigure(can, "current", fill="red"))
    tkitembind(can, "label", "<Any-Leave>",
               function() tkitemconfigure(can, "current", fill=p$col))
    tkitembind(can, "label", "<1>", pDown)
    tkitembind(can, "label", "<ButtonRelease-1>",
               function() {tkdtag(can, "selected"); tkdelete(can, "ptr")})
    
    tkbind(can, "<B1-Motion>", pMove)
}
