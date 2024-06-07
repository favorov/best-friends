.onAttach <- function(libname, pkgname) {
    version <- packageDescription("best.friends", fields="Version")
    packageStartupMessage(paste("Welcome to best.friends, version", version,
        "(A ver\u00E3o chegou, e tu?) and stay tuned!")
    )
}
# \u00E1 is á
# \u00E3 is ã
# for detools::check