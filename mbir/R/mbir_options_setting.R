## set default options for mbir_options:
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mbir <- list(
    mbir.verbose = FALSE,
    mbir.plot = FALSE
  )
  toset <- !(names(op.mbir) %in% names(op))
  if (any(toset)) options(op.mbir[toset])



  invisible()
}
