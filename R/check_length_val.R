check_length_val <- function(length_x, n, header, .abort = rlang::abort) {
  if (all(length_x %in% c(1L, n))) return()

  if (n == 1) {
    glubort(
      header, "must be length 1", "not", plu::stick(length_x),
      .abort = .abort
    )
  } else {
    glubort(
      header, "must be length", n, "or one, not", plu::stick(length_x),
      .abort = .abort
    )
  }
}
