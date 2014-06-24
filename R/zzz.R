.onLoad <- function(libname, pkgname) { }

# revised .onAttach to work with devtools
.onAttach <- function(theLib, pkgname, ...) { 
  pkgdesc <- packageDescription(pkgname, lib.loc = theLib)
  builddate <- gsub(';.*$', '', pkgdesc[['Date']])
  packageStartupMessage(paste("rethinking (Version ", pkgdesc[['Version']], ", packaged: ", builddate, ")", sep = ""))
} 

## .onAttach <- function(...) {
##   theLib <- dirname(system.file(package = "rethinking"))
##   pkgdesc <- packageDescription("rethinking", lib.loc = theLib)
##   builddate <- gsub(';.*$', '', pkgdesc$Packaged)
##   packageStartupMessage(paste("rethinking (Version ", pkgdesc$Version, ", packaged: ", builddate, ")", 
## } 
