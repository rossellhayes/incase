abort_msg <- function(...) {
  rlang::abort(message = c(...))
}

assert_logical_lhs <- function(query, quos_pairs) {
  illogical <- !vapply(query, is.logical, logical(1))

  if (any(illogical)) {
    illogical_lhs <- vapply(
      quos_pairs[illogical],
      function(x) {rlang::as_label(x[["lhs"]])},
      character(1)
    )

    abort_msg(
      "Each formula's left hand side must evaluate to a logical vector",
      x = paste(
        plu::stick(plu::more(code(illogical_lhs), 5, "{left hand} side")),
        plu::ral("{does|do}"), "not evaluate to a logical vector."
      )
    )
  }
}

assert_two_sided <- function(fs, fn) {
  nfs <- Filter(
    function(fs) !rlang::is_formula(fs, lhs = TRUE) && !rlang::is_quosure(fs),
    fs
  )

  if (length(nfs)) {
    abort_msg(
      paste("Each argument to", code(fn), "must be a two-sided formula"),
      x = paste(
        plu::stick(plu::more(code(nfs), 5, "argument")),
        plu::ral("is {not} a {two-sided} formula.", nfs)
      )
    )
  }
}

assert_no_preserve_without_pipe <- function(preserve, fn) {
  if (preserve) {
    abort_msg(
      paste(
        "A vector must be piped into", code(fn),
        "to use", code("preserve")
      ),
      paste("Try using", code("default"), "instead")
    )
  }
}

assert_length <- function(fs) {
  if (!length(fs)) rlang::abort("No cases provided")
}

warn_if_default <- function(default) {
  if (!is.null(default) && !is.na(default)) {
    rlang::warn(
      paste(
        code("default"), "will have no effect if", code("preserve"),
        "is", code("TRUE")
      )
    )
  }
}

code <- function(x) {
  x <- encodeString(x, quote = "`")
  x <- paste0("\033[90m", x, "\033[39m")
  x
}
