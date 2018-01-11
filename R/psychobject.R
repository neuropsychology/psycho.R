#' Creates or tests for objects of mode "psychobject".
#'
#' @param x an arbitrary R object.
#'
#' @export
is.psychobject <- function(x) inherits(x, "psychobject")
