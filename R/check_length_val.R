check_length_val <- function(length_x, n, header, .abort = rlang::abort) {
  if (all(length_x %in% c(1L, n))) return()

  if (n == 1) {
    abort_msg(
      paste(header, "must be length 1, not", plu::stick(length_x))
    )
  } else {
    abort_msg(
      paste(header, "must be length", n, "or 1, not", plu::stick(length_x))
    )
  }
}
