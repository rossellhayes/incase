#' Case statements returning a list
#'
#' These functions are equivalent to [in_case()], [switch_case()],
#' [grep_case()], [fn_case()], and [fn_switch_case()] but return
#' [lists][list].
#'
#' This can be useful when returning a non-[atomic][is.atomic()] value and/or
#' when you want to create a list column inside a [tibble][tibble::tibble].
#'
#' @inheritParams fn_case
#' @inheritParams in_case
#'
#' @return A list of length 1 or n, matching the length of the logical
#'   input vector.
#'
#' @seealso [in_case()], [switch_case()], [grep_case()], [fn_case()], and
#'   [fn_case_fct()] on which these functions are based.
#'
#' @export
#' @example examples/in_case_list.R

in_case_list <- function(..., preserve = FALSE, default = NA) {
  dots <- allow_dot_aliases(compact_list(...))
  inputs <- in_case_setup(dots, preserve = preserve, fn = "in_case_list()")

  replace(
    inputs$fs, inputs$x, default, preserve,
    list        = TRUE,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
  )
}

#' @rdname in_case_list
#' @export

switch_case_list <- function(x, ..., preserve = FALSE, default = NA) {
  fn_case_list(
    x  = x,
    fn = `%in%`,
    ...,
    preserve = preserve,
    default  = default
  )
}

#' @rdname in_case_list
#' @export

grep_case_list <- function(x, ..., preserve = FALSE, default = NA) {
  fn_case_list(
    x  = x,
    fn = function(x, pattern, ...) grepl(pattern, x, ...),
    ...,
    preserve = preserve,
    default  = default
  )
}

#' @rdname in_case_list
#' @export

fn_case_list <- function(x, fn, ..., preserve = FALSE, default = NA) {
  dots <- allow_dot_aliases(compact_list(...))
  inputs <- fn_case_setup(dots)

  replace(
    inputs$fs, x, default, preserve, fn, inputs$args, list = TRUE,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
  )
}

#' @rdname in_case_list
#' @export

fn_switch_case_list <- function(x, fn, ..., preserve = FALSE, default = NA) {
  inputs <- fn_switch_case_setup(
    ...,
    fn          = fn,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
  )

  do.call(
    switch_case_list,
    c(
      list(x = x), inputs$fs, inputs$args,
      list(preserve = preserve, default = default)
    )
  )
}
