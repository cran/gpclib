.onLoad <- function(lib, pkg) {
    if(!require(methods))
        stop(sQuote("methods"), " package is required for ",
             sQuote("gpclib"))
}

.onAttach <- function(lib, pkg) {
    ver <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "Version")
    cat(paste("General Polygon Clipper Library for R (version ",
              as.character(ver), ")\n", sep=""))
    cat("\tType", sQuote("class ? gpc.poly"), "for help\n")
}
