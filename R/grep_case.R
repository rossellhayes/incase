#' Switch-style recoding of values with string pattern matching
#'
#' @param x A vector
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas or named arguments.
#'
#'   - **Formulas**: Elements of `x` that match the [regex] pattern on the
#'   left hand side (LHS) of formulas will be replaced with the value in the
#'   right hand side (RHS).
#'   The LHS must evaluate to a character string.
#'   The RHS must be of length one.
#'   `NULL` inputs are ignored.
#'
#'   - **Named arguments**: named arguments are passed to [grepl()].
#'
#' @inheritParams switch_case
#'
#' @return A vector of the same length as `x`.
#'
#' @seealso [grep_case_fct()] to return a factor and
#'   [grep_case_list()] to return a list
#'
#'   [fn_case()], to apply a function other than `grepl()` to each case
#'
#'   [switch_case()] to recode values with exact matching
#'
#'   [in_case()], a pipeable alternative to [dplyr::case_when()]
#'
#'   [switch()] and [grepl()], which inspired this function
#'
#' @export
#'
#' @example examples/grep_case.R

grep_case <- function(
  x,
  ...,
  .preserve = FALSE,
  .default = NA,
  .exhaustive = FALSE,
  preserve = deprecated(),
  default = deprecated()
) {
  fn_case(
    x = x,
    fn = grepl_any,
    ...,
    .preserve = .preserve,
    .default = .default,
    .exhaustive = .exhaustive,
    preserve = preserve,
    default = default
  )
}

grepl_any <- function(x, pattern, ...) {
  logical <- vapply(pattern, grepl, logical(length(x)), x, ...)
  logical <- matrix(logical, nrow = length(x))
  apply(logical, 1, or_reduce)
}

or_reduce <- function(logical) {
  Reduce(`|`, logical)
}
