validate_case_length <- function(query, value, fs) {
  lhs_lengths <- lengths(query)
  rhs_lengths <- lengths(value)
  all_lengths <- unique(c(lhs_lengths, rhs_lengths))

  if (length(all_lengths) == 0) {
    return(0)
  } else if (length(all_lengths) == 1) {
    return(all_lengths[[1]])
  }

  non_atomic_lengths <- all_lengths[all_lengths != 1]
  len                <- non_atomic_lengths[[1]]

  if (length(non_atomic_lengths) == 1) {return(len)}

  inconsistent_lengths <- non_atomic_lengths[non_atomic_lengths != len]

  lhs_problems         <- lhs_lengths %in% inconsistent_lengths
  rhs_problems         <- rhs_lengths %in% inconsistent_lengths
  problems             <- lhs_problems | rhs_problems

  abort_msg(
    paste0(
      "All formulas' right-hand sides must be the same length (", value(len),
      ") or ", value("1"), ":"
    ),
    check_length_val(
      vapply(fs[problems], rlang::as_label, character(1)),
      inconsistent_lengths,
      len
    )
  )
}

check_length_val <- function(formulas, length_x, n) {
  if (n > 1) {n <- paste(value(n), "or", value("1"))}

  out <- paste0(
    code(formulas), " should be length ", n, ", not ", value(length_x)
  )
  names(out) <- rep("x", length(out))
  out
}

