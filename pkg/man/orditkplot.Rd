\name{orditkplot}
\alias{orditkplot}

\title{ Ordination Plot with Movable Lables }
\description{
  Function \code{orditkplot} produces an editable ordination plot with
  points and labels. The labels can be moved with mouse, and the edited
  plot can be exported to an encapsulated postscript file.
}
\usage{
orditkplot(x, display = "species", width, col = "black",
   bg = "transparent", diam = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An ordination result or any other object that
    \code{\link{scores}} can handle. }
  \item{display}{ Type of \code{\link{scores}} displayed. }
  \item{width}{ Width of the plot in inches (default supplied). }
  \item{col, bg}{ Line and fill colours of points. }
  \item{diam}{ Diameter of points in pixels.}
  \item{\dots}{ Other arguments to \code{\link{scores}}. }
}
\details{
  Function \code{orditkplot} uses \pkg{tcltk} to draw Tcl/Tk based
  ordination graphics with points and labels. The points are fixed, but
  the labels can be moved with mouse to better positions, and the final
  plot can be exported to a postscript file. The axes will have equal
  aspect ratio. Technically, the plot is a Tcl/Tk canvas
  (\code{\link[tcltk]{tkcanvas}}). 

  Only point colours and sizes can be defined in the command, but
  you can use graphical parameters (\code{\link{par}}) to change the
  graphics. At the moment, the following graphical parameters are
  recognized: \code{bg} to change the colour of canvas background, 
  \code{fg}, \code{col.axis} and \code{col.lab} to change
  axis and box colours, \code{col} to change the label colours,
  \code{mar} to change the width plot margins, \code{lwd} to change the
  width of the box around plot. Most graphical \code{\link{par}}ameters
  are not yet recognized. Most notably, the font types and sizes are not
  yet handled. The \code{\link[tcltk]{tkcanvas}} text cannot be rotated,
  and therefore vertical axis is not labelled, and \code{las}
  \code{\link{par}}ameter will not be honoured. 
}
\value{
  Function returns nothing useful directly, but you can save the edited
  graph into encapsulated postscript. At the moment, the fonts are
  handled nicely only in MacOS X.
}

\author{ Jari Oksanen }
\note{
  Function is preliminary and it should improve a lot. Fonts should be
  handled. Postscript output decently works in MacOS only. Environmental
  variables should be added.

  You need \pkg{tcltk} and \R must have been configured with
  \code{\link{capabilities}} for \code{tcltk} when building the binary.
  Depending on your OS, you may need to start X11 and set the display
  before loading \pkg{tcltk} and starting the function (for instance,
  with \code{Sys.setenv("DISPLAY"=":0")}). See
  \code{link[tcltk]{tcltk-package}}. 

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
