.onAttach <- function(libname, pkgname) {
       version <- packageDescription("best.friens.of", field="Version")
       packageStartupMessage(paste("Welcome to best.friands.of version", version,"stay tuned!"))
}
