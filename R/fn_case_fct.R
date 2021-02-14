#' Case statements returning a factor
#'
#' These functions are equivalent to [switch_case()], [grep_case()], and
#' [fn_case()], but return [factors][factor] with their levels determined by the
#' order of their case statements.
#'
#' @inheritParams fn_case
#'
#' @return A factor vector of length 1 or n, matching the length of the logical
#'   input or output vectors.
#'   Levels are determined by the order of inputs to `...`.
#'   Inconsistent lengths will generate an error.
#'
#' @seealso [switch_case()], [grep_case()], and [fn_case()], on which these
#'   functions are based.
#'
#' @export
#' @example examples/fn_case_fct.R

switch_case_fct <- function(x, ..., preserve = FALSE, default = NA) {
  fn_case_fct(
    x  = x,
    fn = `%in%`,
    ...,
    preserve = preserve,
    default  = default
  )
}

#' @rdname switch_case_fct
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

#' @rdname switch_case_fct
#' @export

fn_case_fct <- function(x, fn, ..., preserve = FALSE, default = NA) {
  fn    <- rlang::as_function(fn)
  input <- compact_null(rlang::list2(...))
  fs    <- Filter(rlang::is_formula, input)
  args  <- input[!input %in% fs]

  n <- length(fs)
  if (n == 0) rlang::abort("No cases provided")

  query       <- vector("list", n)
  value       <- vector("list", n)
  default_env <- rlang::caller_env()

  quos_pairs  <- Map(
    function(x, i) {
      validate_formula(
        x, i, default_env = default_env, dots_env = rlang::current_env()
      )
    },
    fs, seq_along(fs)
  )

  for (i in seq_len(n)) {
    pair       <- quos_pairs[[i]]
    value[[i]] <- rlang::eval_tidy(pair[["rhs"]], env = default_env)

    query[[i]] <- rlang::eval_tidy(pair[["lhs"]], env = default_env)
    query[[i]] <- do.call(fn, c(list(x, query[[i]]), args))

    if (!is.logical(query[[i]])) {
      glubort(
        "Each formula's left hand side must evaluate to a logical vector:",
        cross_bullet(), code(rlang::as_label(pair[["lhs"]])),
        "does not evaluate to a logical vector."
      )
    }
  }

  levels <- as.character(c(value, recursive = TRUE))

  if (preserve) {
    warn_if_default(default)
    query[[n + 1]] <- TRUE
    value[[n + 1]] <- x
  }

  value      <- lapply(value, as.character)
  m          <- validate_case_when_length(query, value, fs)
  out        <- as.character(rep_len(default, m))
  replaced   <- rep(FALSE, m)

  for (i in seq_len(length(query))) {
    out      <- replace_with(out, query[[i]] & !replaced, value[[i]])
    replaced <- replaced | (query[[i]] & !is.na(query[[i]]))
  }

  out <- factor(out, levels = c(levels, out[!out %in% levels]))
  out
}
