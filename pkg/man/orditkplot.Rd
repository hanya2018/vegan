\name{orditkplot}
\alias{orditkplot}
\alias{plot.orditkplot}

\title{ Ordination Plot with Movable Labels }
\description{
  Function \code{orditkplot} produces an editable ordination plot with
  points and labels. The labels can be moved with mouse, and the edited
  plot can be exported to an encapsulated postscript file.
}
\usage{
orditkplot(x, display = "species", choices = 1:2, width, xlim, ylim, 
   tcex = 0.8, pcol, pbg, pcex = 0.7, labels,  ...)
\method{plot}{orditkplot}(x, ...)
}

\arguments{ 
  \item{x}{An ordination result or any other object that
    \code{\link{scores}} can handle, or for the \code{plot} function the
    object dumped from the interactive \code{orditkplot} session.}
  \item{display}{ Type of \code{\link{scores}} displayed. }
  \item{choices}{ Axes displayed.}
  \item{width}{ Width of the plot in inches; defaults to the current
    width of graphical device. }
  \item{xlim, ylim}{x and y limits for plots: points outside these
    limits will be completely removed.}
  \item{tcex}{Character expansion for text labels.}
  \item{pcol, pbg}{ Line and fill colours of points. Defaults \code{pcol="black"} 
    and \code{pbg="transparent"}. } 
  \item{pcex}{Expansion factor for point size.}  
  \item{labels}{Labels used instead of row names.}
  \item{\dots}{ Other arguments passed to the function. These can be
    graphical parameters (see \code{\link{par}}) used in the plot, or
    extra arguments to \code{\link{scores}}. These arguments are
    ignored in \code{plot}. }  
}

\details{ Function \code{orditkplot} uses \pkg{tcltk} to draw Tcl/Tk
  based ordination graphics with points and labels. The points are
  fixed, but the labels can be moved with mouse to better
  positions. The edited plot can be copied to an encapsulated
  postscript (eps) file using standard Tcl/Tk utilities. The
  faithfullness of this copy seems to be very system
  dependent. Alternatively, the plot can be exported via \R standard
  devices into eps, pdf, png, jpeg or bmp file (not all types are
  available in all platforms), or to editable \code{\link{xfig}} file.
  Finally, the edited coordinates of labels and points can be dumped
  to an \R object for further processin, and \code{plot.orditkplot}
  function can be used to display the results. For faithtfull
  replication of the plot, the graph must have similar dimensions as
  the \code{orditkplot} canvas had originally.

  The produced plot will have equal aspect ratio. The width of the
  horizontal axis is fixed, but vertical axes will be scaled to needed
  height, and you can use scrollbar to move vertically if the whole
  canvas does not fit the window. If you use dumped labels in ordinary
  \R plots, your plot must have the same dimensions as the
  \code{orditkplot} canvas to have identical location of the labels.

  The plot is a Tcl/Tk canvas, but the function tries to replicate
  standard graphical device of the platform, and it honours several
  graphical parameters (see \code{\link{par}}).  Many of the graphical
  parameters can be given on the command line, and they will be passed
  to the function without influencing other graphical devices in \R.
  At the moment, the
  following graphical parameters are honoured: \code{bg}, \code{cex},
  \code{cex.axis}, \code{cex.lab}, \code{col} (for labels),
  \code{col.axis}, \code{col.lab}, \code{family} (for font faces),
  \code{fg}, \code{font}, \code{font.axis}, \code{font.lab},
  \code{lheight}, \code{lwd} (for the box), \code{mar}, \code{mex},
  \code{mgp}, \code{ps}, \code{tcl}. These can be set with
  \code{\link{par}}, and they also will influence other plots similarly.
  
  The \code{\link[tcltk]{tkcanvas}} text cannot be rotated,
  and therefore vertical axis is not labelled, and \code{las}
  \code{\link{par}}ameter will not be honoured. 
}

\value{ Function returns nothing useful directly, but you can save the
  edited graph to a file or dump the edited positions to an \R session
  for further processing and plotting.  
}

\author{ Jari Oksanen }
\note{
  You need \pkg{tcltk} and \R must have been configured with
  \code{\link{capabilities}} for \code{tcltk} when building the binary.
  Depending on your OS, you may need to start X11 and set the display
  before loading \pkg{tcltk} and starting the function (for instance,
  with \code{Sys.setenv("DISPLAY"=":0")}). See
  \code{\link[tcltk]{tcltk-package}}. 

}
\seealso{ \code{\link{ordiplot}}, \code{\link{plot.cca}},
  \code{\link{ordirgl}} and \code{\link{orditorp}} for alternative
  ordination plots, and \code{\link{scores}} for extracting ordination
  scores.  }
\examples{
## The example needs user interaction and is not executed directly.
## It should work when pasted to the window.
\dontrun{
data(varespec)
ord <- cca(varespec)
orditkplot(ord)
}
}
\keyword{ iplot }
\keyword{ dynamic }
