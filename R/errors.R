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

style <- function(x, quote, color) {

}

style <- function(x, quote, color) {
  if (!is.null(quote)) {x <- encodeString(x, quote = quote)}
  if (rlang::is_installed("crayon")) {
    x <- do.call(color, list(x), envir = asNamespace("crayon"))
  }
  x
}

code  <- function(x) {style(x, "`", "silver")}
value <- function(x) {style(x, NULL, "blue")}
