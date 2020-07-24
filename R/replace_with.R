replace_with <- function(x, i, val, name, reason = NULL) {
  check_length_val(length(val), length(x), name, reason)

  i[is.na(i)] <- FALSE

  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }

  x
}
