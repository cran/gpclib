## gpclib:  General Polygon Clipping library for R
## Copyright (C) 2003 Roger D. Peng <rpeng@stat.ucla.edu>


## R functions for using GPC library and manipulating polygons

## The format of the data files must be the following:
##
## <number of contours>
## <number of points in first contour>
## x1  y1
## x2  y2
## ...
## <number of points in second contour>
## x1  y1
## x2  y2
## ...
##

## require(methods)

## Got this trick from the Bioconductor packages.  .First.lib calls
## .initRgpc and creates all of the methods in the correct place.

.initRgpc <- function(where) {
    setClass("gpc.poly",
             representation(pts = "list"),
             prototype(pts = list()), where = where)

    setMethod("show", "gpc.poly",
              function(object) {
                  cat("GPC Polygon\n")
                  cat("   Num. Contours: ", length(object@pts), "\n")
                  if(length(object@pts) == 1)
                      cat("   Num. Vertices: ", length(object@pts[[1]]$x), "\n")
                  bbox <- get.bbox(object)
                  cat("   BBox (X): ", bbox$x[1], "-->", bbox$x[2], "\n")
                  cat("   BBox (Y): ", bbox$y[1], "-->", bbox$y[2], "\n")
              }, where = where)

    setGeneric("get.bbox", function(x)
               standardGeneric("get.bbox"), where = where)

    setMethod("get.bbox", signature(x = "gpc.poly"),
              function(x) {
                  pts <- x@pts
                  x <- unlist(lapply(pts, "[[", "x"))
                  y <- unlist(lapply(pts, "[[", "y"))
                  
                  if(!is.null(x))
                      xlim <- range(x)
                  else
                      xlim <- c(NA, NA)
                  if(!is.null(y))
                      ylim <- range(y)
                  else
                      ylim <- c(NA, NA)
                  list(x = xlim, y = ylim)
              }, where = where)

    setGeneric("intersect", where = where)
    setGeneric("union", where = where)
    setGeneric("setdiff", where = where)
    setGeneric("plot", where = where)

    setMethod("plot", "gpc.poly",
              function(x, y, poly.args = list(), xlab = "X", ylab = "Y", asp = 1,
                       add = FALSE, ...) {
                  if(!add) {
                      bbox <- get.bbox(x)
                      plot(0, 0, ylim = bbox$y, xlim = bbox$x, type="n",
                           xlab = xlab, ylab = ylab, asp = asp, ...)
                  }
                  invisible(lapply(x@pts, function(p) {
                      do.call("polygon", append(list(x = p), poly.args))
                  }))
              }, where = where)

    setMethod("intersect", signature(x = "gpc.poly", y = "gpc.poly"),
              function(x, y) {
                  subject <- as(x, "numeric")
                  clip <- as(y, "numeric")
                  vec <- .Call("gpc_polygon_intersect", subject, clip);

                  if(identical(vec, 0)) 
                      rval <- new("gpc.poly")
                  else 
                      rval <- as(vec, "gpc.poly")
                  rval
              }, where = where)

    setMethod("union", signature(x = "gpc.poly", y = "gpc.poly"),
              function(x, y) {
                  subject <- as(x, "numeric")
                  clip <- as(y, "numeric")
                  vec <- .Call("gpc_polygon_union", subject, clip);

                  if(identical(vec, 0)) 
                      rval <- new("gpc.poly")
                  else                   
                      rval <- as(vec, "gpc.poly")
                  rval
              }, where = where)

    setMethod("setdiff", signature(x = "gpc.poly", y = "gpc.poly"),
              function(x, y) {
                  subject <- as(x, "numeric")
                  clip <- as(y, "numeric")
                  vec <- .Call("gpc_polygon_difference", subject, clip);

                  if(identical(vec, 0)) 
                      rval <- new("gpc.poly")
                  else 
                      rval <- as(vec, "gpc.poly")
                  rval
              }, where = where)

    setMethod("[", "gpc.poly",
              function(x, i, j, ..., drop = FALSE) {
                  new("gpc.poly", pts = x@pts[i])
              }, where = where)

    setAs("matrix", "gpc.poly",
          function(from, to) {
              if(ncol(from) > 2)
                  stop("Matrix must have 2 columns")
              p <- list(x = from[,1], y = from[,2])
              new("gpc.poly", pts = list(p))
          }, where = where)

    setAs("data.frame", "gpc.poly",
          function(from, to) {
              as(as.matrix(from), "gpc.poly")
          }, where = where)

    setAs("gpc.poly", "matrix",
          function(from, to) {
              if(length(from@pts) > 1)
                  stop("Can only convert a single contour into a matrix")
              pts <- from@pts[[1]]
              m <- cbind(x = pts$x, y = pts$y)
          }, where = where)

    ## These two functions are needed for the intersect/union/setdiff
    ## methods.  They convert the "gpc.poly" object into just a long
    ## vector of numbers (and back).

    setAs("numeric", "gpc.poly", 
          function(from, to) {
              ## The shortest a vector can be is 8 numbers:  1. Num. Contours;
              ## 2. Num pts for first contour; and three vertices
              if(length(from) < 8)
                  stop("Numeric vector not long enough")
              expand.poly <- function(x) {
                  ## `x' is just a long vector of numbers with a special format
                  num.contours <- x[1]; x <- x[-1]
                  polyfile <- x
                  poly <- vector("list", length = num.contours)
                  
                  for(i in 1:num.contours) {
                      npts <- polyfile[1]; polyfile <- polyfile[-1]
                      m <- matrix(polyfile[1:(2*npts)], byrow = TRUE, ncol = 2)
                      poly[[i]] <- list(x = m[,1], y = m[,2])
                      polyfile <- polyfile[-(1:(2*npts))]
                  }
                  poly
              }
              new("gpc.poly", pts = expand.poly(from))
          }, where = where)

    setAs("gpc.poly", "numeric",
          function(from, to) {
              flatten.poly <- function(poly) {
                  num.contours <- length(poly@pts)
                  flat <- lapply(poly@pts, function(p)
                             {
                                 v <- as.vector(t(cbind(p$x, p$y)))
                                 c(length(p$x), v)
                             })
                  c(num.contours, unlist(flat))
              }
              flatten.poly(from)
          }, where = where)

    ## Miscellaneous Utility Functions
    
    setGeneric("append.poly", function(x, y)
               standardGeneric("append.poly"), where = where)

    setMethod("append.poly",
              signature(x = "gpc.poly", y = "gpc.poly"),
              function(x, y) {
                  newpts <- append(x@pts, y@pts)
                  new("gpc.poly", pts = newpts)
              }, where = where)

    setGeneric("scale.poly", function(x, ...)
               standardGeneric("scale.poly"), where = where)
    
    setMethod("scale.poly", signature(x = "gpc.poly"), 
              function(x, xscale, yscale = xscale, ...) {
                  x@pts <- lapply(x@pts, function(p)
                              {
                                  p$x <- p$x / xscale
                                  p$y <- p$y / yscale
                                  p
                              })
                  x
              }, where = where)
    
    ## Compute the area of each polygon in the polygon set contained in `object'.
    ## If `total' is TRUE then the areas of each contour are summed.
    setGeneric("area.poly", function(object, ...)
               standardGeneric("area.poly"), where = where)

    setMethod("area.poly", signature(object = "gpc.poly"),
              function(object, ...) {
                  area <- function(x.mat) {
                      if(nrow(x.mat) < 3) 
                          return(0);   
                      x.segmat <- cbind(x.mat, rbind(x.mat[2:nrow(x.mat), ], x.mat[1, ]));
                      abs(sum(x.segmat[,1] * x.segmat[,4] - x.segmat[,3] * x.segmat[,2])) / 2
                  }
                  a <- sapply(object@pts, function(p) area(cbind(p$x, p$y)))
                  sum(a)
              }, where = where)

    setGeneric("get.pts", function(object)
               standardGeneric("get.pts"), where = where)

    setMethod("get.pts", signature(object = "gpc.poly"),
              function(object) {
                  object@pts
              }, where = where)
}


## Read a polygon from a file

read.polyfile <- function(filename) {
    polyfile <- scan(filename, quiet = TRUE)
    as(polyfile, "gpc.poly")
}

## Write a "gpc.poly" object to a text file

write.polyfile <- function(poly, filename = "GPCpoly.txt") {    
    if(!is(poly, "gpc.poly"))
        stop("`poly' should be of class \"gpc.poly\"")
    outfile <- file(filename)
    open(outfile, open = "w")

    num.contours <- length(poly@pts)
    cat(num.contours, "\n", file = outfile)
    
    for(i in 1:num.contours) {
        m <- as(poly[i], "matrix")
        cat(nrow(m), "\n", file = outfile, append = TRUE)
        write(t(m), file = outfile, ncolumns = 2, append = TRUE)
    }
    close(outfile)
}











