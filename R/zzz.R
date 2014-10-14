.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\nbig.data.frame is in development.\n")
}
.onLoad <- function(libname, pkgname) {
  options(bigmemory.print.warning=TRUE)
  options(bigmemory.typecast.warning=FALSE)
  options(bigmemory.allow.dimnames=TRUE)
}
