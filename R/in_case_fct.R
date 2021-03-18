#' Case statements returning a factor
#'
#' These functions are equivalent to [switch_case()], [grep_case()], and
#' [fn_case()], but return [factors][factor] with their levels determined by the
#' order of their case statements.
#'
#' @inheritParams fn_case
#' @inheritParams in_case
#'
#' @return A factor vector of length 1 or n, matching the length of the logical
#'   input or output vectors.
#'   Levels are determined by the order of inputs to `...`.
#'   Inconsistent lengths will generate an error.
#'
#' @seealso [in_case()], [switch_case()], [grep_case()], and [fn_case()],
#'   on which these functions are based.
#'
#' @export
#' @example examples/in_case_fct.R

in_case_fct <- function(..., preserve = FALSE, default = NA) {
  ellipsis <- compact_null(rlang::list2(...))

  if (!rlang::is_formula(ellipsis[[1]])) {
    fs <- ellipsis[-1]
    x  <- ellipsis[[1]]
  } else {
    fs <- ellipsis
    x  <- NULL
    assert_no_preserve_without_pipe(preserve, "in_case_fct()")
  }

  assert_two_sided(fs, "in_case_fct()")

  replace(
    fs, x, default, preserve, factor = TRUE,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
  )
}

#' @rdname in_case_fct
#' @export

switch_case_fct <- function(x, ..., preserve = FALSE, default = NA) {
  fn_case_fct(
    x  = x,
    fn = `%in%`,
    ...,
    preserve = preserve,
    default  = default
  )
}

#' @rdname in_case_fct
#' @export

grep_case_fct <- function(x, ..., preserve = FALSE, default = NA) {
  fn_case_fct(
    x  = x,
    fn = function(x, pattern, ...) grepl(pattern, x, ...),
    ...,
    preserve = preserve,
    default  = default
  )
}

#' @rdname in_case_fct
#' @export

fn_case_fct <- function(x, fn, ..., preserve = FALSE, default = NA) {
  input <- compact_null(rlang::list2(...))
  fs    <- Filter(rlang::is_formula, input)
  args  <- input[!input %in% fs]

  replace(
    fs, x, default, preserve, fn, args,
    factor      = TRUE,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
  )
}
