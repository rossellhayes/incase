#' Switch-style recoding of values
#'
#' @param x A vector
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas.
#'   Elements of `x` that match the left hand side (LHS) will be replaced with
#'   the value in the right hand side (RHS).
#'
#'   The LHS must evaluate to an atomic vector.
#'
#'   The RHS must be of length one.
#'
#'   `NULL` inputs are ignored.
#'
#' @param preserve If `TRUE`, unmatched elements of `x` will be returned
#'   unmodified.
#'   (The elements may have their type coerced to be compatible with
#'   replacement values.)
#'   If `FALSE`, unmatched elements of `x` will be replaced with `default`.
#'   Defaults to `FALSE`.
#' @param default If `preserve` is `FALSE`, a value to replace unmatched
#'   elements of `x`.
#'   Defaults to `NA`.
#'
#' @return A vector of the same length as `x`.
#'
#' @seealso [grep_case()] to recode values with string pattern matching
#'
#'   [in_case()], a pipeable alternative to [dplyr::case_when()]
#'
#'   [if_case()], a pipeable alternative to [dplyr::if_else()]
#'
#'   [switch()], which inspired this function
#'
#' @export
#'
#' @example examples/switch_case.R

switch_case <- function(x, ..., preserve = FALSE, default = NA) {
  fs  <- compact_null(rlang::list2(...))
  env <- lapply(fs, environment)
  fs  <- lapply(fs, format)
  fs  <- paste(deparse(substitute(x)), "%in%", fs)
  fs  <- mapply(stats::as.formula, fs, env)

  if (preserve) {
    warn_if_default(default)
    fs[[length(fs) + 1]] <- TRUE ~ x
  }

  in_case(!!!fs, default = default)
}
