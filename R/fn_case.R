#' Case statements applying a function to all inputs
#'
#' @inheritParams switch_case
#'
#' @param fn A function to apply to the left-hand side of each formula in `...`
#'
#'   Either a quoted or unquoted function name, an anonymous [`function`], or
#'   a [purrr-style formula][rlang::as_function()].
#'
#'   The function should take two inputs, the first being `x` and the second
#'   being the left-hand side of the formula.
#'   The function should return a logical vector, either of length 1 or the same
#'   length as `x`.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas or named arguments.
#'
#'   - **Formulas**: Elements of `x` that return `TRUE` when passed to `fn` with
#'   the left hand side (LHS) of each formula will be replaced with the value in
#'   the right hand side (RHS).
#'   The LHS must evaluate to a logical vector when passed to `fn` with `x`.
#'   The RHS must be of length 1 or the same length as all other RHS.
#'
#'   - **Named arguments**: named arguments are passed as additional arguments
#'   to the function `fn`.
#'
#' @return A vector of length 1 or n, matching the length of the logical input
#'   or output vectors.
#'   Inconsistent lengths will generate an error.
#'
#' @seealso [fn_case_fct()] to return a factor and
#'   [fn_case_list()] to return a list
#'
#'   [fn_switch_case()], which applies a function to each formula's LHS,
#'   but not `x`
#'
#'   [switch_case()], a simpler alternative for exact matching
#'
#'   [grep_case()], a simpler alternative for [regex] pattern matching
#'
#'   [in_case()], a pipeable alternative to [dplyr::case_when()]
#'
#' @export
#' @example examples/fn_case.R

fn_case <- function(
  x,
  fn,
  ...,
  .preserve = FALSE,
  .default = NA,
  .exhaustive = FALSE,
  preserve = deprecated(),
  default = deprecated()
) {
  .preserve <- coalesce_deprecated(.preserve, preserve)
  .default <- coalesce_deprecated(.default, default)

  dots <- compact_list(...)
  inputs <- fn_case_setup(dots)

  replace(
    fs = inputs$fs,
    x = x,
    .default = .default,
    .preserve = .preserve,
    .exhaustive = .exhaustive,
    fn = fn,
    args = inputs$args,
    default_env = first_incase_frame_parent(),
    current_env = first_incase_frame()
  )
}

fn_case_setup <- function(dots) {
  fs    <- Filter(rlang::is_formula, dots)
  args  <- dots[!dots %in% fs]

  list(fs = fs, args = args)
}
