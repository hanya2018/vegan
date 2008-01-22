\name{orditkplot}
\alias{orditkplot}

\title{ Ordination Plot with Movable Labels }
\description{
  Function \code{orditkplot} produces an editable ordination plot with
  points and labels. The labels can be moved with mouse, and the edited
  plot can be exported to an encapsulated postscript file.
}
\usage{
orditkplot(x, display = "species", cex = 0.8, width, col = "black",
   bg = "transparent", diam = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An ordination result or any other object that
    \code{\link{scores}} can handle. }
  \item{display}{ Type of \code{\link{scores}} displayed. }
  \item{cex}{Character expansion for text labels.}
  \item{width}{ Width of the plot in inches (default supplied). }
  \item{col, bg}{ Line and fill colours of points. }
  \item{diam}{ Diameter of points in pixels.}
  \item{\dots}{ Other arguments to \code{\link{scores}}. }
}
\details{
  Function \code{orditkplot} uses \pkg{tcltk} to draw Tcl/Tk based
  ordination graphics with points and labels. The points are fixed, but
  the labels can be moved with mouse to better positions, and the final
  plot can be exported to a postscript file or the new positions of the
  labels can be save in the \R session and used in ordinary
  \code{\link{text}} function to label an existing plot. 

  The produced plot will have equal aspect ratio. The width of the
  horizontal axis is fixed, but vertical axes will be scaled to needed
  height, and you can use scrollbar to move vertically if the whole
  canvas does not fit the window. If you use dumped labels in ordinary
  \R plots, your plot must have the same dimensions as the
  \code{orditkplot} canvas to have identical location of the labels.

  The plot is a Tcl/Tk canvas, but the function tries to replicate
  standard graphical device of the platform, and it honours several
  graphical parameters (see \code{\link{par}}).  At the moment, the
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
\value{
  Function returns nothing useful directly, but you can save the edited
  graph into encapsulated postscript. How postscript is produced is very
  dependent on the platform, and the results are not satisfactory on all
  platforms.
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
