.onAttach <- function(libname, pkgname) {
       version <- packageDescription("differential.coverage", field="Version")
       packageStartupMessage(paste("Welcome to best.friands.of version", version,"stay tuned!"))
}
