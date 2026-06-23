.onAttach <- function(libname, pkgname) {
  version <- tryCatch(
    utils::packageDescription("cohort", fields = "Version"),
    warning = function(w) NA_character_
  )
  packageStartupMessage(
    "cohort ", version, "\n",
    "https://www.rwhite.no/cohort/"
  )
}
