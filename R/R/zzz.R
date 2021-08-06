.onAttach <- function(libname, pkgname) {
       version <- packageDescription("best.friends", field="Version")
       packageStartupMessage(paste("Welcome to best.friends, version", version," and stay tuned!"))
}
