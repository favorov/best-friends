.onAttach <- function(libname, pkgname) {
       version <- packageDescription("best.friends.of", field="Version")
       packageStartupMessage(paste("Welcome to best.friands.of version", version,"stay tuned!"))
}
