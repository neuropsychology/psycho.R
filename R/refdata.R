#' Create a reference grid.
#'
#' Create a reference grid.
#'
#' @param df The dataframe.
#' @param target String or list of strings to indicate target columns. Can be "all".
#' @param length.out Length of numeric target variables.
#' @param factors Type of summary for factors. Can be "combination" or "reference".
#' @param numerics Type of summary for numerics Can be "combination", any function ("mean", "median", ...) or a value.
#'
#' @examples
#' library(psycho)
#'
#' df <- psycho::affective
#' newdata <- refdata(df, target="Sex")
#' newdata <- refdata(df, target="Sex", factors="combinations")
#' newdata <- refdata(df, target=c("Sex", "Salary", "Tolerating"), length.out=3)
#' newdata <- refdata(df, target=c("Sex", "Salary", "Tolerating"), numerics=0)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom purrr keep
#' @import tidyr
#' @export
refdata <- function(df, target="all", length.out=10, factors="reference", numerics="mean") {

  # Target
  if (all(target == "all") | ncol(df) == 1) {
    return(.refdata_target(target = df[c(names(df))], length.out = length.out))
  }

  target_df <- .refdata_target(target = df[c(target)], length.out = length.out)

  # Rest
  df_rest <- df[!names(df) %in% c(target)]
  var_order <- names(df_rest)

  facs <- purrr::discard(df_rest, is.numeric)
  nums <- purrr::keep(df_rest, is.numeric)


  smart_summary <- function(x, numerics) {
    if (is.numeric(x)) {
      fun <- paste0(numerics, "(x)")
      out <- eval(parse(text = fun))
    } else if (is.factor(x)) {
      out <- levels(x)[1]
    } else if (is.character(x)) {
      out <- unique(x)[1]
    } else if (is.logical(x)) {
      out <- unique(x)[1]
    } else {
      warning("Argument is not numeric nor factor: returning NA.")
      out <- NA
    }
    return(out)
  }


  if (factors == "reference") {
    facs <- dplyr::summarise_all(facs, smart_summary)
  } else {
    facs <- tidyr::expand_(facs, names(facs))
  }

  if (is.numeric(numerics)) {
    nums[1, ] <- numerics
    nums <- nums[1, ]
  } else if(numerics == "combination"){
    nums <- tidyr::expand_(nums, names(nums))
  } else {
    nums <- dplyr::summarise_all(nums, smart_summary, numerics)
  }


  if (nrow(facs) == 0 | ncol(facs) == 0) {
    refrest <- nums
  } else if (nrow(nums) == 0 | ncol(nums) == 0) {
    refrest <- facs
  } else {
    refrest <- merge(facs, nums)
  }

  refrest <- refrest[var_order]
  refdata <- merge(target_df, refrest)

  return(refdata)
}










#' @keywords internal
.refdata_target <- function(target, length.out=10) {
  at_vars <- names(target)
  at_df <- data.frame()
  for (var in at_vars) {
    ref_var <- .refdata_var(x = target[[var]], length.out = length.out, varname = var)
    if (nrow(at_df) == 0) {
      at_df <- ref_var
    } else {
      at_df <- merge(at_df, ref_var)
    }
  }
  return(at_df)
}


















#' @keywords internal
.refdata_var <- function(x, length.out=10, varname=NULL) {
  if (is.numeric(x)) {
    out <- data.frame(seq(min(x),
      max(x),
      length.out = length.out
    ))
  } else if (is.factor(x)) {
    out <- data.frame(levels(x))
  } else if (is.character(x)) {
    x <- as.factor(x)
    out <- data.frame(levels(x))
  } else {
    warning("Argument is not numeric nor factor: returning NA.")
    out <- NA
    return()
  }

  if (is.null(varname)) {
    names(out) <- "x"
  } else {
    names(out) <- varname
  }
  return(out)
}
