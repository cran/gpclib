## require(methods)

.First.lib <- function(lib, pkg) {
    library.dynam("gpclib", pkg, lib)
    where <- match(paste("package:", pkg, sep = ""), search())
    .initRgpc(where)
    cat("General Polygon Clipper Library for R (version 1.0.0)\n")
    cat("\tSee ?gpc.poly for help\n")
}
