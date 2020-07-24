check_length_val <- function(
  length_x, n, header, reason = NULL, .abort = rlang::abort
) {
  if (all(length_x %in% c(1L, n))) return()

  if (is.null(reason)) {
    reason <- ""
  }
  else {
    reason <- paste0("(", reason, ")")
  }

  if (n == 1) {
    glubort(
      header, "must be length 1", reason, "not", plu::stick(length_x),
      .abort = .abort
    )
  }
  else {
    glubort(
      header, "must be length", n, reason, "or one, not", plu::stick(length_x),
      .abort = .abort
    )
  }
}
