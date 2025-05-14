.onAttach <- function(libname, pkgname) {
    version <- packageDescription("friends.test", fields="Version")
    packageStartupMessage(paste("Welcome to friends.test, version", version,
        "(Lebre de mayo) and stay tuned!")
    )
}
# \u00E1 is á
# \u00E3 is ã
# \u00E7 is ç

# for detools::check