## require(methods)

.First.lib <- function(lib, pkg) {
    library.dynam("gpclib", pkg, lib)
    where <- match(paste("package:", pkg, sep = ""), search())
    .initRgpc(where)
    desc <- readLines(file.path(lib, pkg, "DESCRIPTION"))
    verstr <- desc[grep("^Version:", desc)]
    ver <- sub("^Version: +", "", verstr)
    cat(paste("General Polygon Clipper Library for R (version ", ver, ")\n", sep=""))
    cat("\tSee ?gpc.poly for help\n")
}
