## set default options for mbir_options:
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mbir <- list(
    mbir.verbose = FALSE,
    mbir.plot = FALSE,
    mech_decisions = list(
      strong_alpha = .005,
      moderate_alpha = .05,
      weak_alpha = .25),
    clin_decisions = list(
      use_direction = "positive",
      benefit_alpha = 0.25,
      harm_alpha = .005
    ),
    conf.level = .95
  )
  toset <- !(names(op.mbir) %in% names(op))
  if (any(toset)) options(op.mbir[toset])



  invisible()
}
