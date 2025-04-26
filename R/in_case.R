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
#' @param .preserve If `TRUE`, unmatched elements of the input will be
#'   returned unmodified.
#'   (The elements may have their type coerced to be compatible with
#'   replacement values.)
#'   If `FALSE`, unmatched elements of the input will be replaced
#'   with `.default`.
#'   Defaults to `FALSE`.
#' @param .default If `.preserve` is `FALSE`, a value to replace unmatched
#'   elements of the input.
#'   Defaults to `NA`.
#' @param preserve,default `r lifecycle::badge("deprecated")`
#'   Deprecated in favor of `.preserve` and `.default`
#'
#' @return A vector of length 1 or n, matching the length of the logical input
#'   or output vectors.
#'   Inconsistent lengths will generate an error.
#'
#' @seealso [in_case_fct()] to return a factor and
#'   [in_case_list()] to return a list
#'
#'   [switch_case()] a simpler alternative for when each case involves
#'   [`==`] or [`%in%`]
#'
#'   [fn_case()], a simpler alternative for when each case uses the
#'   same function
#'
#'   [if_case()], a pipeable alternative to [dplyr::if_else()]
#'
#'   [dplyr::case_when()], from which this function is derived
#'
#' @export
#'
#' @example examples/in_case.R

in_case <- function(
  ...,
  .preserve = FALSE,
  .default = NA,
  preserve = deprecated(),
  default = deprecated()
) {
  .preserve <- coalesce_deprecated(.preserve, preserve)
  .default <- coalesce_deprecated(.default, default)

  dots <- compact_list(...)
  inputs <- in_case_setup(dots, .preserve = .preserve, fn = "in_case")

  replace(
    fs = inputs$fs,
    x = inputs$x,
    .default = .default,
    .preserve = .preserve,
    default_env = first_incase_frame_parent(),
    current_env = first_incase_frame()
  )
}

in_case_setup <- function(dots, .preserve, fn) {
  if (length(dots) == 0) {
    return(list(fs = list(), x = vector()))
  }

  if (!rlang::is_formula(dots[[1]])) {
    fs <- dots[-1]
    x  <- dots[[1]]
  } else {
    fs <- dots
    x  <- NULL
    assert_no_preserve_without_pipe(.preserve, fn)
  }

  assert_two_sided(fs, fn)

  list(fs = fs, x = x)
}

assert_no_preserve_without_pipe <- function(.preserve, fn) {
  if (.preserve) {
    cli::cli_abort(c(
      "The first argument to {.fn {fn}} must be a vector
      to use the {.arg .preserve} argument.",
      "*" = "Try using {.arg .default} instead."
    ))
  }
}

#' @importFrom plu ral
assert_two_sided <- function(fs, fn) {
  nfs <- Filter(
    function(fs) !rlang::is_formula(fs, lhs = TRUE) && !rlang::is_quosure(fs),
    fs
  )

  if (length(nfs)) {
    cli::cli_abort(c(
      "Each argument to {.fn {fn}} must be a two-sided formula.",
      x = "{.code {nfs}} {plu::ral('is {not} a {two-sided} formula.', nfs)}."
    ))
  }
}
