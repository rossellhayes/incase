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
#'   Levels are determined by the order of inputs to `...`.
#'   Inconsistent lengths will generate an error.
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

  dots <- allow_dot_aliases(compact_list(...))
  inputs <- in_case_setup(dots, .preserve = .preserve, fn = "in_case_fct")

  replace(
    fs          = inputs$fs,
    x           = inputs$x,
    .default    = .default,
    .preserve   = .preserve,
    factor      = TRUE,
    .ordered    = .ordered,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
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
  preserve = deprecated(),
  default = deprecated(),
  ordered = deprecated()
) {
  .preserve <- coalesce_deprecated(.preserve, preserve)
  .default <- coalesce_deprecated(.default, default)
  .ordered <- coalesce_deprecated(.ordered, ordered)

  fn_case_fct(
    x  = x,
    fn = `%in%`,
    ...,
    .preserve = .preserve,
    .default  = .default,
    .ordered  = .ordered
  )
}

#' @rdname in_case_fct
#' @export

grep_case_fct <- function(
  x,
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

  fn_case_fct(
    x  = x,
    fn = grepl_any,
    ...,
    .preserve = .preserve,
    .default  = .default,
    .ordered  = .ordered
  )
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
  preserve = deprecated(),
  default = deprecated(),
  ordered = deprecated()
) {
  .preserve <- coalesce_deprecated(.preserve, preserve)
  .default <- coalesce_deprecated(.default, default)
  .ordered <- coalesce_deprecated(.ordered, ordered)

  dots <- allow_dot_aliases(compact_list(...))
  inputs <- fn_case_setup(dots)

  replace(
    inputs$fs, x, .default, .preserve, fn, inputs$args,
    factor      = TRUE,
    .ordered    = .ordered,
    default_env = rlang::caller_env(),
    current_env = rlang::current_env()
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
  preserve = deprecated(),
  default = deprecated(),
  ordered = deprecated()
) {
  .preserve <- coalesce_deprecated(.preserve, preserve)
  .default <- coalesce_deprecated(.default, default)
  .ordered <- coalesce_deprecated(.ordered, ordered)

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
      list(.preserve = .preserve, .default = .default, .ordered = .ordered)
    )
  )
}
