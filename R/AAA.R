# .RESEMBLE_CACHE <- new.env(FALSE, parent = globalenv())

.onAttach <- function(lib, pkg) {
  pkg_v <- pkg_info()

  mss <- paste0(
    "\033[34m",
    pkg, " version ",
    paste(pkg_v[1:2], collapse = " \U002D\U002D "),
    "\033[39m"
  )
  mss2 <- paste0(
    "\033[34mcheck the package repository at: ",
      pkg_v[, "URL"],
    "\033[39m"
  )
  packageStartupMessage(mss)
  packageStartupMessage(mss2)
}

# .onUnload <- function(libpath) {
#     rm(.RESEMBLE_CACHE)
# }
