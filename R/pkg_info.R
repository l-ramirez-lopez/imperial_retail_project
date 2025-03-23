#' @title Get the package version info
#' @description returns package info.
#' @param pkg the package name i.e "rma"
#' @keywords internal
pkg_info <- function(pkg = "rma") {
  fld <- c("Version", "Config/VersionName", "URL")
  pinfo <- read.dcf(system.file("DESCRIPTION", package = pkg), fields = fld)
  pinfo
}
