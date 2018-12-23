#' Citations of loaded packages.
#'
#' Get the citations of loaded packages.
#'
#' @param session A `devtools::sessionInfo()` object.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' cite_packages(sessionInfo())
#' }
#' 
#' @author \href{https://github.com/DominiqueMakowski}{Dominique Makowski}
#'
#' @export
cite_packages <- function(session) {
  pkgs <- session$otherPkgs
  citations <- c()
  for (pkg_name in names(pkgs)) {
    pkg <- pkgs[[pkg_name]]

    citation <- format(citation(pkg_name))[[2]] %>%
      stringr::str_split("\n") %>%
      flatten() %>%
      paste(collapse = "SPLIT") %>%
      stringr::str_split("SPLITSPLIT")

    i <- 1
    while (stringr::str_detect(citation[[1]][i], "To cite ")) {
      i <- i + 1
    }


    citation <- citation[[1]][i] %>%
      stringr::str_remove_all("SPLIT") %>%
      stringr::str_trim() %>%
      stringr::str_squish()

    citations <- c(citations, citation)
  }
  return(data.frame("Packages" = citations))
}
