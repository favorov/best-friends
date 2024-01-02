.onAttach <- function(libname, pkgname) {
       version <- packageDescription("best.friends", fields="Version")
       packageStartupMessage(paste("Welcome to best.friends, version", version,"(Feliz Ano Novo) and stay tuned!"))
}
