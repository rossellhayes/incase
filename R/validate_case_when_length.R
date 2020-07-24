validate_case_when_length <- function(query, value, fs) {
  lhs_lengths <- lengths(query)
  rhs_lengths <- lengths(value)
  all_lengths <- unique(c(lhs_lengths, rhs_lengths))

  if (length(all_lengths) <= 1) {
    return(all_lengths[[1]])
  }

  non_atomic_lengths <- all_lengths[all_lengths != 1]
  len                <- non_atomic_lengths[[1]]

  if (length(non_atomic_lengths) == 1) {
    return(len)
  }

  inconsistent_lengths <- non_atomic_lengths[-1]
  lhs_problems         <- lhs_lengths %in% inconsistent_lengths
  rhs_problems         <- rhs_lengths %in% inconsistent_lengths
  problems             <- lhs_problems | rhs_problems

  glubort(
    plu::stick(vapply(fs[problems], rlang::as_label, character(1)), code),
    check_length_val(
      inconsistent_lengths, len, header = NULL, .abort = identity
    )
  )
}
