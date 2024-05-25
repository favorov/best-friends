.onAttach <- function(libname, pkgname) {
    version <- packageDescription("best.friends", fields="Version")
    packageStartupMessage(paste("Welcome to best.friends, version", version,
        "(A Primvera est\u00e1 chegando, e tu?) and stay tuned!")
    )
}
# \u00e1 is á