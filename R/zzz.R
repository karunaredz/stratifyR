# =============================================================================
# zzz.R  -  stratifyR 2.1
#
# Registers the knit_print() method for "summary.stratifyR" with knitr, which is
# only suggested, so the method is registered when knitr is loaded.
# =============================================================================

.onLoad <- function(libname, pkgname) {
  .s3_register("knitr::knit_print", "summary.stratifyR", knit_print.summary.stratifyR)
  invisible()
}

.s3_register <- function(generic, class, method) {
  pieces  <- strsplit(generic, "::", fixed = TRUE)[[1]]
  package <- pieces[[1]]; generic <- pieces[[2]]
  register <- function(...) {
    envir <- asNamespace(package)
    if (exists(generic, envir = envir, inherits = FALSE))
      registerS3method(generic, class, method, envir = envir)
  }
  if (isNamespaceLoaded(package)) register()
  setHook(packageEvent(package, "onLoad"), register)
  invisible()
}
