replace_with <- function(x, i, val, name) {
  check_length_val(length(val), length(x), name)

  i[is.na(i)] <- FALSE

  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }

  x
}
