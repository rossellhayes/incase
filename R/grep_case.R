#' Switch-style recoding of values with string pattern matching
#'
#' @param x A vector
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> A sequence of two-sided
#'   formulas.
#'   Elements of `x` that match the left hand side (LHS) will be replaced with
#'   the value in the right hand side (RHS).
#'
#'   The LHS must evaluate to a character string.
#'
#'   The RHS must be of length one.
#'
#'   `NULL` inputs are ignored.
#'
#' @inheritParams switch_case
#'
#' @return A vector of the same length as `x`.
#'
#' @seealso [switch_case()] to recode values with exact matching
#'
#'   [in_case()], a pipeable alternative to [dplyr::case_when()]
#'
#'   [if_case()], a pipeable alternative to [dplyr::if_else()]
#'
#'   [switch()] and [grepl()], which inspired this function
#'
#' @export
#'
#' @example examples/grep_case.R

grep_case <- function(x, ..., preserve = FALSE, default = NA) {
  fs  <- compact_null(rlang::list2(...))

  args <- fs[names(fs) %in% names(formals(grepl))]
  fs   <- fs[!fs %in% args]

  if (length(args)) {
    args <- paste(names(args), "=", args)
  }

  env <- lapply(fs, environment)

  fs <- vapply(fs, format, character(1))
  fs <- strsplit(fs, "~")
  fs <- do.call("cbind", fs)

  fs <- paste0(
    "grepl(", fs[1, ], ", ", deparse(substitute(x)), ",", args, ") ~", fs[2, ]
  )
  fs  <- mapply(stats::as.formula, fs, env)

  if (preserve) {
    warn_if_default(default)
    fs[[length(fs) + 1]] <- TRUE ~ x
  }

  in_case(!!!fs, default = default)
}
