#' Pipe-friendly vectorized if
#'
#' Compared to [dplyr::if_else()], this function is easier to use with a pipe.
#' A vector piped into this function will be quietly ignored.
#' This allows [magrittr][magrittr::magrittr] dots to be used in arguments
#' without requiring workarounds like wrapping the function in braces.
#'
#' This function is also less strict than [dplyr::if_else()].
#' If `true`, `false`, and `missing` are different types, they are silently
#' coerced to a common type.
#'
#' @param condition Logical vector
#' @param true,false,missing Values to use for `TRUE`, `FALSE`, and `NA` values
#'   of `condition`.
#'   They must be either the same length as `condition`, or length 1.
#' @param ... Values passed to `...` produce an error.
#'   This facilitates the quiet ignoring of a piped vector.
#'
#' @return Where `condition` is `TRUE`, the matching value from `true`;
#'   where it's `FALSE`, the matching value from `false`;
#'   and where it's `NA`, the matching value from `missing`.
#'
#' @seealso [in_case()], a pipeable alternative to [dplyr::case_when()]
#'
#'   [switch_case()], a reimplementation of [switch()]
#'
#'   [dplyr::if_else()], from which this function is derived
#'
#' @importFrom rlang %||%
#' @importFrom plu ral
#' @export
#'
#' @example examples/if_case.R

if_case <- function(condition, true, false, missing = NA, ...) {
  ellipsis <- list(...)

  if (try(identical(sys.call()[[2]], rlang::sym(".")), silent = TRUE)) {
    unspecified <- setdiff(names(formals()), names(sys.call()))
    ellipsis    <- list(...)

    if (length(ellipsis)) {
      from <- lapply(
        as.list(unspecified[-1]),
        function(x) {
          if (x == "...") return(ellipsis[[1]])
          eval(parse(text = x))
        }
      )

      to <- unspecified[-length(unspecified)]

      for (i in seq_along(to)) assign(to[[i]], from[[i]])

      ellipsis <- ellipsis[-1]
    }
  }

  if (length(ellipsis) > 0) {
    cli::cli_abort(c(
      "Arguments must not be passed to {.arg ...} in {.fun if_case}.",
      x = "{.code {ellipsis}} {plu::ral('was', ellipsis)} passed to {.arg ...}."
    ))
  }

  if (!is.logical(condition)) {
    cli::cli_abort(c(
      "{.arg condition} must be {.type {logical(2)}}.",
      x = "{.arg condition} is {.type {condition}}."
    ))
  }

  # Implement lazy-ish evaluation of output vectors
  if (!isTRUE(any(condition)))   {true    <- NULL}
  if (!isTRUE(any(!condition)))  {false   <- NULL}
  if (!isTRUE(anyNA(condition))) {missing <- NULL}

  true    <- true    %||% false %||% missing
  false   <- false   %||% true  %||% missing
  missing <- missing %||% true  %||% false

  check_condition_lengths(
    condition, list(true = true, false = false, missing = missing)
  )

  if (is.atomic(true) && is.atomic(false) && is.atomic(missing)) {
    common <- c(true, false, missing, recursive = TRUE)
  } else {
    common <- list()
  }

  storage.mode(true)    <- storage.mode(common)
  storage.mode(false)   <- storage.mode(common)
  storage.mode(missing) <- storage.mode(common)

  out <- true[rep(NA, length(condition))]
  out <- replace_with(out, condition,        true,    "true")
  out <- replace_with(out, !condition,       false,   "false")
  out <- replace_with(out, is.na(condition), missing, "missing")

  out
}

check_condition_lengths <- function(condition, replacements) {
  replacement_lengths <- lengths(replacements)
  problem             <- !replacement_lengths %in% c(0, 1, length(condition))

  if (any(problem)) {
    msg <- sprintf(
      "{.arg %s} is length {.val {%d}}.",
      names(replacements[problem]),
      replacement_lengths[problem]
    )
    names(msg) <- rep("x", length(msg))

    cli::cli_abort(c(
      "Replacement vectors must be the same length as {.arg condition},
      ({.val {length(condition)}}) or length {.val {1}}.",
      msg
    ))
  }
}
