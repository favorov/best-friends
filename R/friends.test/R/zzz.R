.onAttach <- function(libname, pkgname) {
    version <- packageDescription("best.friends", fields="Version")
    packageStartupMessage(paste("Welcome to best.friends, version", version,
        "(Lebre de mar\u00E7o) and stay tuned!")
    )
}
# \u00E1 is á
# \u00E3 is ã
# \u00E7 is ç

# for detools::check