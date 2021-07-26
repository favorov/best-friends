.onAttach <- function(libname, pkgname) {
       version <- packageDescription("best.friends.of", field="Version")
       packageStartupMessage(paste("Welcome to best.friends.of version", version," and stay tuned!"))
}
