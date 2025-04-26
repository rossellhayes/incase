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

in_case_list <- function(
  ...,
  .preserve = FALSE,
  .default = NA,
  preserve = deprecated(),
  default = deprecated()
) {
  .preserve <- coalesce_deprecated(.preserve, preserve)
  .default <- coalesce_deprecated(.default, default)

  dots <- compact_list(...)
  inputs <- in_case_setup(dots, .preserve = .preserve, fn = "in_case_list")

  replace(
    fs = inputs$fs,
    x = inputs$x,
    .default = .default,
    .preserve = .preserve,
    list = TRUE,
    default_env = first_incase_frame_parent(),
    current_env = first_incase_frame()
  )
}

#' @rdname in_case_list
#' @export

switch_case_list <- function(
  x,
  ...,
  .preserve = FALSE,
  .default = NA,
  preserve = deprecated(),
  default = deprecated()
) {
  fn_case_list(
    x  = x,
    fn = `%in%`,
    ...,
    .preserve = .preserve,
    .default = .default,
    preserve = preserve,
    default = default,
  )
}

#' @rdname in_case_list
#' @export

grep_case_list <- function(
  x,
  ...,
  .preserve = FALSE,
  .default = NA,
  preserve = deprecated(),
  default = deprecated()
) {
  fn_case_list(
    x  = x,
    fn = function(x, pattern, ...) grepl(pattern, x, ...),
    ...,
    .preserve = .preserve,
    .default  = .default,
    preserve = preserve,
    default = default,
  )
}

#' @rdname in_case_list
#' @export

fn_case_list <- function(
  x,
  fn,
  ...,
  .preserve = FALSE,
  .default = NA,
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
    fn = fn,
    args = inputs$args,
    list = TRUE,
    default_env = first_incase_frame_parent(),
    current_env = first_incase_frame()
  )
}

#' @rdname in_case_list
#' @export

fn_switch_case_list <- function(
  x,
  fn,
  ...,
  .preserve = FALSE,
  .default = NA,
  preserve = deprecated(),
  default = deprecated()
) {
  eval.parent(fn_switch_case_call("switch_case_list", fn, ...))
}
