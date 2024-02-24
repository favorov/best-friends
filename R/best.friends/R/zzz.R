.onAttach <- function(libname, pkgname) {
       version <- packageDescription("best.friends", fields="Version")
       packageStartupMessage(paste("Welcome to best.friends, version", version,"(A Primvera está chegando, e tu?) and stay tuned!"))
}
