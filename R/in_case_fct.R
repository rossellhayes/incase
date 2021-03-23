#' Case statements returning a factor
#'
#' These functions are equivalent to [in_case()], [switch_case()],
#' [grep_case()], [fn_case()], and [fn_switch_case()] but return
#' [factors][factor] with their levels determined by the order of their
#' case statements.
#'
#' @inheritParams fn_case
#' @inheritParams in_case
#' @param ordered A logical.
#'   If [`TRUE`], returns an [ordered] factor.
#'   If [`FALSE`], returns an unordered factor.
#'
#' @return A factor vector of length 1 or n, matching the length of the logical
#'   input or output vectors.
#'   Levels are determined by the order of inputs to `...`.
#'   Inconsistent lengths will generate an error.
#'
#' @seealso [in_case()], [switch_case()], [grep_case()], [fn_case()], and
#'   [fn_case_fct()] on which these functions are based.
#'
#' @export
#' @example examples/in_case_fct.R

in_case_fct <- function(..., preserve = FALSE, default = NA, ordered = FALSE) {
  inputs <- in_case_setup(..., preserve = preserve, fn = "in_case_fct()")

  replace(
    fs          = inputs$fs,
    x           = inputs$x,
    default     = default,
    preserve    = preserve,
    factor      = TRUE,
    ordered     = ordered,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
  )
}

#' @rdname in_case_fct
#' @export

switch_case_fct <- function(
  x, ..., preserve = FALSE, default = NA, ordered = FALSE
) {
  fn_case_fct(
    x  = x,
    fn = `%in%`,
    ...,
    preserve = preserve,
    default  = default,
    ordered  = ordered
  )
}

#' @rdname in_case_fct
#' @export

grep_case_fct <- function(
  x, ..., preserve = FALSE, default = NA, ordered = FALSE
) {
  fn_case_fct(
    x  = x,
    fn = function(x, pattern, ...) grepl(pattern, x, ...),
    ...,
    preserve = preserve,
    default  = default,
    ordered  = ordered
  )
}

#' @rdname in_case_fct
#' @export

fn_case_fct <- function(
  x, fn, ..., preserve = FALSE, default = NA, ordered = FALSE
) {
  input <- compact_list(...)
  fs    <- Filter(rlang::is_formula, input)
  args  <- input[!input %in% fs]

  replace(
    fs, x, default, preserve, fn, args,
    factor      = TRUE,
    ordered     = ordered,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
  )
}

#' @rdname in_case_fct
#' @export

fn_switch_case_fct <- function(
  x, fn, ..., preserve = FALSE, default = NA, ordered = FALSE
) {
  inputs <- fn_switch_case_setup(
    ...,
    fn          = fn,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
  )

  do.call(
    switch_case_fct,
    c(
      list(x = x), inputs$fs, inputs$args,
      list(preserve = preserve, default = default, ordered = ordered)
    )
  )
}
