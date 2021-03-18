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
#' @seealso [fn_switch_case()], which applies a function to each formula's LHS,
#'   but not `x`
#'
#'   [switch_case()], a simpler alternative for exact matching
#'
#'   [grep_case()], a simpler alternative for [regex] pattern matching
#'
#' @export
#' @example examples/fn_case.R

fn_case <- function(x, fn, ..., preserve = FALSE, default = NA) {
  input <- compact_null(rlang::list2(...))
  fs    <- Filter(rlang::is_formula, input)
  args  <- input[!input %in% fs]

  replace(
    fs, x, default, preserve, fn, args,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
  )
}
