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
#' @param .preserve If `TRUE`, unmatched elements of `x` will be returned
#'   unmodified.
#'   (The elements may have their type coerced to be compatible with
#'   replacement values.)
#'   If `FALSE`, unmatched elements of `x` will be replaced with `.default`.
#'   Defaults to `FALSE`.
#' @param .default If `.preserve` is `FALSE`, a value to replace unmatched
#'   elements of `x`.
#'   Defaults to `NA`.
#'
#' @param .exhaustive If `TRUE`, unmatched elements of `x` will result in
#'   an error.
#'   This can be useful to ensure you aren't accidentally forgetting to recode
#'   any values.
#'   Defaults to `FALSE`.
#'
#'   Note that if `.preserve` is `TRUE`,
#'   `.exhaustive` will never have any effect.
#'
#' @param preserve,default `r lifecycle::badge("deprecated")`
#'   Deprecated in favor of `.preserve` and `.default`
#'
#' @return A vector of the same length as `x`.
#'
#' @seealso [switch_case_fct()] and [fn_switch_case_fct()] to return a factor
#'   and [switch_case_list()] and [fn_switch_case_list()] to return a list
#'
#'   [grep_case()] to recode values with string pattern matching
#'
#'   [fn_case()], which applies a function to both `x` and each formula's LHS
#'
#'   [in_case()], a pipeable alternative to [dplyr::case_when()]
#'
#'   [switch()] and [`%in%`], which inspired this function
#'
#' @export
#'
#' @example examples/switch_case.R

switch_case <- function(
  x,
  ...,
  .preserve = FALSE,
  .default = NA,
  .exhaustive = FALSE,
  preserve = deprecated(),
  default = deprecated()
) {
  fn_case(
    x  = x,
    fn = `%in%`,
    ...,
    .preserve = .preserve,
    .default  = .default,
    .exhaustive = .exhaustive,
    preserve = preserve,
    default = default
  )
}

#' @rdname switch_case
#' @export

fn_switch_case <- function(
  x,
  fn,
  ...,
  .preserve = FALSE,
  .default = NA,
  preserve = deprecated(),
  default = deprecated()
) {
  eval.parent(fn_switch_case_call("switch_case", fn, ...))
}

fn_switch_case_call <- function(
  switch_case_fn,
  fn,
  ...,
  call = rlang::caller_call(),
  current_fn = rlang::caller_fn()
) {
  args <- as.list(call)[-1]

  dots <- compact_list(...)
  dots_idx <- which(args %in% dots)

  dots <- fn_switch_case_setup(
    dots,
    fn          = fn,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
  )

  args[dots_idx] <- dots
  args <- args[!vapply(args, rlang::is_missing, logical(1))]

  fn_idx <- which(args == rlang::call_match(call, current_fn)[["fn"]])
  args <- args[-fn_idx]

  rlang::call2(switch_case_fn, !!!args)
}

fn_switch_case_setup <- function(dots, fn, default_env, current_env) {
  are_formulas <- vapply(dots, rlang::is_formula, logical(1))

  fs    <- dots[are_formulas]
  args  <- dots[!are_formulas]

  assert_length(fs, call = current_env)

  pairs <- extract_formula_pairs(
    fs,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env(),
    logical_lhs = FALSE
  )

  fs <- Map(
    function(fs, query, value) {
      rlang::f_lhs(fs) <- do.call(rlang::as_function(fn), c(list(query), args))
      rlang::f_rhs(fs) <- value
      fs
    },
    fs, pairs$query, pairs$value
  )

  dots[are_formulas] <- fs
  dots[!are_formulas] <- list(rlang::missing_arg())

  dots
}
