#' Case statements returning a factor
#'
#' These functions are equivalent to [in_case()], [switch_case()],
#' [grep_case()], [fn_case()], and [fn_switch_case()] but return
#' [factors][factor] with their levels determined by the order of their
#' case statements.
#'
#' @inheritParams fn_case
#' @inheritParams in_case
#' @param .ordered A logical.
#'   If [`TRUE`], returns an [ordered] factor.
#'   If [`FALSE`], returns an unordered factor.
#' @param preserve,default,ordered `r lifecycle::badge("deprecated")`
#'   Deprecated in favor of `.preserve`, `.default`, and `.ordered`
#'
#' @return A factor vector of length 1 or n, matching the length of the logical
#'   input or output vectors.
#'   Levels are determined by the order of inputs to `...` and `.default`.
#'   Inconsistent lengths will generate an error.
#'
#'   The position of the `.default` argument is taken into account when setting
#'   factor levels in `*_case_fct()` functions.
#'   For example, if the `.default` argument is given before any case
#'   statements, the default value will be the first level of the factor;
#'   if the `.default` argument is positioned in between two case statements,
#'   the default value will be ordered in between the value of the two
#'   case statements.
#'
#' @seealso [in_case()], [switch_case()], [grep_case()], [fn_case()], and
#'   [fn_case_fct()] on which these functions are based.
#'
#' @export
#' @example examples/in_case_fct.R

in_case_fct <- function(
  ...,
  .preserve = FALSE,
  .default = NA,
  .ordered = FALSE,
  preserve = deprecated(),
  default = deprecated(),
  ordered = deprecated()
) {
  .preserve <- coalesce_deprecated(.preserve, preserve)
  .default <- coalesce_deprecated(.default, default)
  .ordered <- coalesce_deprecated(.ordered, ordered)

  call <- rlang::current_call()
  dots <- compact_list(...)
  dots_idx <- which(as.list(call) %in% dots)
  .default_idx <- which(names(call) %in% c("default", ".default"))

  inputs <- in_case_setup(dots, .preserve = .preserve, fn = "in_case_fct")

  replace(
    fs = inputs$fs,
    x = inputs$x,
    .default = .default,
    .preserve = .preserve,
    factor = TRUE,
    .ordered = .ordered,
    default_env = first_incase_frame_parent(),
    current_env = first_incase_frame(),
    dots_idx = dots_idx,
    .default_idx = .default_idx
  )
}

#' @rdname in_case_fct
#' @export

switch_case_fct <- function(
  x,
  ...,
  .preserve = FALSE,
  .default = NA,
  .ordered = FALSE,
  .exhaustive = FALSE,
  preserve = deprecated(),
  default = deprecated(),
  ordered = deprecated()
) {
  args <- as.list(rlang::current_call())[-1]

  eval.parent(rlang::call2(fn_case_fct, fn = `%in%`, !!!args))
}

#' @rdname in_case_fct
#' @export

grep_case_fct <- function(
  x,
  ...,
  .preserve = FALSE,
  .default = NA,
  .ordered = FALSE,
  .exhaustive = FALSE,
  preserve = deprecated(),
  default = deprecated(),
  ordered = deprecated()
) {
  args <- as.list(rlang::current_call())[-1]

  eval.parent(rlang::call2(fn_case_fct, fn = grepl_any, !!!args))
}

#' @rdname in_case_fct
#' @export

fn_case_fct <- function(
  x,
  fn,
  ...,
  .preserve = FALSE,
  .default = NA,
  .ordered = FALSE,
  .exhaustive = FALSE,
  preserve = deprecated(),
  default = deprecated(),
  ordered = deprecated()
) {
  .preserve <- coalesce_deprecated(.preserve, preserve)
  .default <- coalesce_deprecated(.default, default)
  .ordered <- coalesce_deprecated(.ordered, ordered)

  call <- rlang::current_call()
  dots <- compact_list(...)
  dots_idx <- which(as.list(call) %in% dots)
  .default_idx <- which(names(call) %in% c("default", ".default"))

  inputs <- fn_case_setup(dots)

  replace(
    fs = inputs$fs,
    x = x,
    .default = .default,
    .preserve = .preserve,
    .exhaustive = .exhaustive,
    fn = fn,
    args = inputs$args,
    factor = TRUE,
    .ordered = .ordered,
    default_env = first_incase_frame_parent(),
    current_env = first_incase_frame(),
    dots_idx = dots_idx,
    .default_idx = .default_idx
  )
}

#' @rdname in_case_fct
#' @export

fn_switch_case_fct <- function(
  x,
  fn,
  ...,
  .preserve = FALSE,
  .default = NA,
  .ordered = FALSE,
  .exhaustive = FALSE,
  preserve = deprecated(),
  default = deprecated(),
  ordered = deprecated()
) {
  eval.parent(fn_switch_case_call(switch_case_fct, fn, ...))
}
