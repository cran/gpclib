\name{gpc.poly-class}
\docType{class}
\alias{gpc.poly-class}
\alias{gpc.poly}
\alias{append.poly}
\alias{area.poly}
\alias{get.bbox}
\alias{scale.poly}
\alias{get.pts}

\title{Class "gpc.poly"}

\description{A class for polygons consisting of multiple contours.}

\section{Objects from the Class}{
  Objects can be created by using the function \code{read.polyfile} or by
  coercing from another data type.  One can also use
  \code{new("gpc.poly", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{pts}:}{Object of class \code{"list"}.  A list of lists,
      actually.  Each sub-list contains the points of a single contour.
      A sub-list has elements \code{x} and \code{y} which contain,
      respectively, the x and y coordinates of the vertices of the contour.}
  }
}

\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "gpc.poly")}: return a subset of
      contours of a polygon as a \code{"gpc.poly"} object.}
    
    \item{append.poly}{\code{signature(x = "gpc.poly", y = "gpc.poly")}:
      join two \code{"gpc.poly"} objects and return the combined polygon
      as a \code{"gpc.poly"} object.}

    \item{area.poly}{\code{signature(object = "gpc.poly")}:
      compute the total area of all contours of a
      polygon.}
    
    \item{get.bbox}{\code{signature(x = "gpc.poly")}: return the
      bounding box of all contours in a polygon.}

    \item{get.pts}{\code{signature(object = "gpc.poly")}: return the
      \code{pts} slot of argument.}

    \item{intersect}{\code{signature(x = "gpc.poly", y = "gpc.poly")}:
      return the intersection of polygons \code{x} and \code{y} as a
      \code{"gpc.poly"} object.}

    \item{plot}{\code{signature(x = "gpc.poly")}: plot a polygon.
      \code{plot} calls \code{polygon} to plot each contour and
      arguments to \code{polygon} can be passed as a list via the
      \code{poly.args} argument for \code{plot}.}
    
    \item{scale.poly}{\code{signature(x = "gpc.poly")}: rescale the x
      and y coordinates of a \code{"gpc.poly"} object.}

    \item{setdiff}{\code{signature(x = "gpc.poly", y = "gpc.poly")}:
      return the (asymmetric) difference of two polygons, i.e. return
      \code{x} \ \code{y}.}

    \item{show}{\code{signature(object = "gpc.poly")}: print a
      \code{"gpc.poly"} object.}

    \item{union}{\code{signature(x = "gpc.poly", y = "gpc.poly")}:
      return the union of two polygons.}

    \item{coerce}{\code{signature(from = "matrix", to = "gpc.poly")}:
      convert a matrix into a polygon with one contour. }
    \item{coerce}{\code{signature(from = "data.frame", to =
	"gpc.poly")}: convert a data frame into a polygon with one contour. }
    \item{coerce}{\code{signature(from = "gpc.poly", to = "matrix")}:
      convert a polygon (with one contour) into a two-column matrix. }
    \item{coerce}{\code{signature(from = "gpc.poly", to = "numeric")}:
      convert a polygon in to a vector (don't use this). }
    \item{coerce}{\code{signature(from = "numeric", to = "gpc.poly")}:
      convert a vector into a polgon (don't use this). }
  }
}
\author{Roger D. Peng; GPC Library written by Alan Murta}

\seealso{\code{\link{read.polyfile}}, \code{\link{write.polyfile}}}

\examples{

## Make some random polygons
set.seed(100)
a <- cbind(rnorm(100), rnorm(100))
a <- a[chull(a), ]

## Convert `a' from matrix to "gpc.poly"
a <- as(a, "gpc.poly")

b <- cbind(rnorm(100), rnorm(100))
b <- as(b[chull(b), ], "gpc.poly")

## More complex polygons with an intersection
p1 <- read.polyfile(system.file("poly-ex/ex-poly1.txt", package = "gpclib"))
p2 <- read.polyfile(system.file("poly-ex/ex-poly2.txt", package = "gpclib"))

## Plot both polygons and highlight their intersection in red
plot(append.poly(p1, p2))
plot(intersect(p1, p2), poly.args = list(col = 2), add = TRUE)

## Highlight the difference p1 \ p2 in green
plot(setdiff(p1, p2), poly.args = list(col = 3), add = TRUE)

## Highlight the difference p2 \ p1 in blue
plot(setdiff(p2, p1), poly.args = list(col = 4), add = TRUE)

## Plot the union of the two polygons
plot(union(p1, p2))

## Take the non-intersect portions and create a new polygon
## combining the two contours
p.comb <- append.poly(setdiff(p1, p2), setdiff(p2, p1))
plot(p.comb, poly.args = list(col = 2, border = 0))

}
\keyword{classes}
