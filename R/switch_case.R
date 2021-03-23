#' Switch-style recoding of values
#'
#' @param x A vector
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas or named arguments.
#'
#'   - **Formulas**: Elements of `x` that match the left hand side (LHS) of
#'   formulas will be replaced with the value in the right hand side (RHS).
#'   The LHS must evaluate to an atomic vector.
#'   The RHS must be of length one.
#'   `NULL` inputs are ignored.
#'
#'   - **Named arguments**: for `fn_switch_case()`, named arguments are passed
#'   to the function `fn`.
#'   For `switch_case()`, named arguments will raise an error.
#'
#' @param fn A function to apply to the left-hand side of each formula in `...`
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
#' @seealso [switch_case_fct()] and [fn_switch_case_fct()] to create a factor
#'
#'   [grep_case()] to recode values with string pattern matching
#'
#'   [fn_case()], which applies a function to both `x` and each formula's LHS
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
  fn_case(
    x  = x,
    fn = `%in%`,
    ...,
    preserve = preserve,
    default  = default
  )
}

#' @rdname switch_case
#' @export

fn_switch_case <- function(x, fn, ..., preserve = FALSE, default = NA) {
  input <- compact_list(...)
  fs    <- Filter(rlang::is_formula, input)
  args  <- input[!input %in% fs]

  assert_length(fs)

  pairs <- extract_formula_pairs(
    fs,
    default_env        = rlang::caller_env(),
    current_env        = rlang::current_env(),
    assert_logical_lhs = FALSE
  )

  fs <- Map(
    function(fs, query, value) {
      rlang::f_lhs(fs) <- do.call(rlang::as_function(fn), c(list(query), args))
      rlang::f_rhs(fs) <- value
      fs
    },
    fs, pairs$query, pairs$value
  )

  do.call(
    switch_case,
    c(list(x = x), fs, args, list(preserve = preserve, default = default))
  )
}
