#' Case statements returning a factor
#'
#' These functions are equivalent to [in_case()], [switch_case()],
#' [grep_case()], [fn_case()], and [fn_switch_case()] but return
#' [factors][factor] with their levels determined by the order of their
#' case statements.
#'
#' @inheritParams fn_case
#' @inheritParams in_case
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

#' @rdname in_case_fct
#' @export

fn_switch_case_fct <- function(x, fn, ..., preserve = FALSE, default = NA) {
  input <- compact_null(rlang::list2(...))
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
    switch_case_fct,
    c(list(x = x), fs, args, list(preserve = preserve, default = default))
  )
}
