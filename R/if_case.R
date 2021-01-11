#' Pipe-friendly vectorized if
#'
#' Compared to [dplyr::if_else()], this function is easier to use with a pipe.
#' A vector piped into this function will be quietly ignored.
#' This allows [magrittr] dots to be used in arguments without requiring
#' workarounds like wrapping the function in braces.
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
#' @export
#'
#' @example examples/if_case.R

if_case <- function(condition, true, false, missing = NA, ...) {
  ellipsis <- list(...)

  if (try(sys.call()[[2]] == ".", silent = TRUE)) {
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

  if (length(ellipsis)) {
    glubort(
      "Arguments must not be passed to", code("..."), "in", code("if_case()"),
      cross_bullet(), plu::stick(plu::more(code(ellipsis), type = "argument")),
      plu::ral("was", ellipsis), "passed to", code("...")
    )
  }

  if (!is.logical(condition)) {
    glubort(
      code("condition"), "must be a logical vector, not",
      code(paste(class(condition), collapse = "/"))
    )
  }

  if (is.atomic(true) && is.atomic(false) && is.atomic(missing)) {
    common <- c(true, false, missing, recursive = TRUE)
  } else {
    common <- list()
  }

  storage.mode(true)    <- storage.mode(common)
  storage.mode(false)   <- storage.mode(common)
  storage.mode(missing) <- storage.mode(common)

  out <- true[rep(NA, length(condition))]
  out <- replace_with(out, condition, true, code("true"))
  out <- replace_with(out, !condition, false, code("false"))
  out <- replace_with(out, is.na(condition), missing, code("missing"))

  out
}
