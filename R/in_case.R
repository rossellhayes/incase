#' A pipe-friendly general vectorized if
#'
#' This function allows you to vectorize multiple if_else() statements.
#' If no cases match, NA is returned.
#' This function derived from [dplyr::case_when()].
#' Unlike [dplyr::case_when()], `in_case()` supports piping elegantly and
#'   attempts to handle inconsistent types (see examples).
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas.
#'   The left hand side (LHS) determines which values match this case.
#'   The right hand side (RHS) provides the replacement value.
#'
#'   The LHS must evaluate to a logical vector.
#'
#'   Both LHS and RHS may have the same length of either 1 or `n`.
#'   The value of `n` must be consistent across all cases.
#'   The case of `n == 0` is treated as a variant of `n != 1`.
#'
#'   `NULL` inputs are ignored.
#'
#' @param preserve If `TRUE`, unmatched elements of the input will be
#'   returned unmodified.
#'   (The elements may have their type coerced to be compatible with
#'   replacement values.)
#'   If `FALSE`, unmatched elements of the input will be replaced
#'   with `default`.
#'   Defaults to `FALSE`.
#' @param default If `preserve` is `FALSE`, a value to replace unmatched
#'   elements of the input.
#'   Defaults to `NA`.
#'
#' @return A vector of length 1 or n, matching the length of the logical input
#'   or output vectors.
#'   Inconsistent lengths will generate an error.
#'
#' @seealso [if_case()], a pipeable alternative to [dplyr::if_else()]
#'
#'   [switch_case()] a simpler alternative for when each case involves
#'   [`==`] or [`%in%`]
#'
#'   [fn_case()], a simpler alternative for when each case uses the
#'   same function
#'
#'   [dplyr::case_when()], from which this function is derived
#'
#' @export
#'
#' @example examples/in_case.R

in_case <- function(..., preserve = FALSE, default = NA) {
  ellipsis <- compact_null(rlang::list2(...))

  if (!rlang::is_formula(ellipsis[[1]])) {
    fs <- ellipsis[-1]

    if (preserve) {
      warn_if_default(default)
      default <- ellipsis[[1]]
    }
  } else {
    fs <- ellipsis

    if (preserve) {
      glubort(
        code("preserve"), " requires a vector to be piped into ",
        code("in_case()"), ":", bullet(), " Try using ", code("default"),
        " instead", .zero = TRUE
      )
    }
  }

  n <- length(fs)
  if (n == 0) rlang::abort("No cases provided")

  nfs <- Filter(
    function(fs) !rlang::is_formula(fs, lhs = TRUE) && !rlang::is_quosure(fs),
    fs
  )

  if (length(nfs)) {
    glubort(
      "Each argument to", code("in_case()"), "must be a two-sided formula:",
      cross_bullet(), plu::stick(nfs, code, max = 5),
      plu::ral("is {not} a {two-sided} formula.", nfs)
    )
  }

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
    query[[i]] <- rlang::eval_tidy(pair[["lhs"]], env = default_env)
    value[[i]] <- rlang::eval_tidy(pair[["rhs"]], env = default_env)

    if (!is.logical(query[[i]])) {
      glubort(
        "Each formula's left hand side must be a logical vector:",
        cross_bullet(), code(rlang::as_label(pair[["lhs"]])),
        "is not a logical vector."
      )
    }
  }

  class      <- class(c(value, recursive = TRUE))
  value      <- lapply(value, `class<-`, class)
  m          <- validate_case_when_length(query, value, fs)
  out        <- rep_len(default, m)
  class(out) <- class
  replaced   <- rep(FALSE, m)

  for (i in seq_len(n)) {
    out      <- replace_with(out, query[[i]] & !replaced, value[[i]])
    replaced <- replaced | (query[[i]] & !is.na(query[[i]]))
  }

  out
}
