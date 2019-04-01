.onAttach <- function(libname, pkgname) {
  packageStartupMessage("message: psycho's `analyze()` is deprecated in favour of the report package. Check it out at https://github.com/easystats/report")
}
