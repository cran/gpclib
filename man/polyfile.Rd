\name{polyfile}
\alias{polyfile}
\alias{read.polyfile}
\alias{write.polyfile}

\title{Read/Write polygon data}
\description{
  Read/Write polygon and contour information from/to a text file.  
}
\usage{
read.polyfile(filename)
write.polyfile(poly, filename = "GPCpoly.txt")
}
\arguments{
  \item{filename}{the name of the file (a character string) from/to which
    data should be read/written.}
  \item{poly}{an object of class \code{"gpc.poly"}}
}
\details{
  The text file representation of a \code{"gpc.poly"} object is the
  following format:
 
  <number of contours>\cr
  <number of points in first contour>\cr
  x1  y1\cr
  x2  y2\cr
  ...\cr
  <number of points in second contour>\cr
  x1  y1\cr
  x2  y2\cr
  ...\cr

  For example, a data file for a polygon with 2 contours (a four-sided
  object and a triangle) might look like:

  2\cr
  4\cr
  1.0 1.0\cr
  1.0 2.0\cr
  3.4 3.21\cr
  10 11.2\cr
  3\cr
  21.0 11.2\cr
  22.3 99.2\cr
  4.5 5.4\cr

  The vertices of the polygon can be ordered either clockwise or
  counter-clockwise.

}

\value{
  \code{read.polyfile} returns an object of class \code{"gpc.poly"}.
  \code{write.polyfile} does not return anything useful.
}
\author{Roger D. Peng}

\examples{
## None right now.
}
\keyword{IO}% at least one, from doc/KEYWORDS
