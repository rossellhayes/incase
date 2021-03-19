#' A pipe-friendly general vectorized if
#'
#' This function allows you to vectorize multiple if_else() statements.
#' If no cases match, NA is returned.
#' This function derived from [dplyr::case_when()].
#' Unlike [dplyr::case_when()], `in_case()` supports piping elegantly and
#'   attempts to handle inconsistent types (see examples).
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas.
#'   The left hand side (LHS) determines which values match this case.
#'   The right hand side (RHS) provides the replacement value.
#'
#'   The LHS must evaluate to a logical vector.
#'
#'   Both LHS and RHS may have the same length of either 1 or `n`.
#'   The value of `n` must be consistent across all cases.
#'   The case of `n == 0` is treated as a variant of `n != 1`.
#'
#'   `NULL` inputs are ignored.
#'
#' @param preserve If `TRUE`, unmatched elements of the input will be
#'   returned unmodified.
#'   (The elements may have their type coerced to be compatible with
#'   replacement values.)
#'   If `FALSE`, unmatched elements of the input will be replaced
#'   with `default`.
#'   Defaults to `FALSE`.
#' @param default If `preserve` is `FALSE`, a value to replace unmatched
#'   elements of the input.
#'   Defaults to `NA`.
#'
#' @return A vector of length 1 or n, matching the length of the logical input
#'   or output vectors.
#'   Inconsistent lengths will generate an error.
#'
#' @seealso [in_case_fct()] to create a factor
#'
#'   [if_case()], a pipeable alternative to [dplyr::if_else()]
#'
#'   [switch_case()] a simpler alternative for when each case involves
#'   [`==`] or [`%in%`]
#'
#'   [fn_case()], a simpler alternative for when each case uses the
#'   same function
#'
#'   [dplyr::case_when()], from which this function is derived
#'
#' @export
#'
#' @example examples/in_case.R

in_case <- function(..., preserve = FALSE, default = NA) {
  ellipsis <- compact_null(rlang::list2(...))

  if (!rlang::is_formula(ellipsis[[1]])) {
    fs <- ellipsis[-1]
    x  <- ellipsis[[1]]
  } else {
    fs <- ellipsis
    x  <- NULL
    assert_no_preserve_without_pipe(preserve, "in_case()")
  }

  assert_two_sided(fs, "in_case()")

  replace(
    fs, x, default, preserve,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
  )
}
