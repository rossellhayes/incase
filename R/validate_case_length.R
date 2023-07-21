validate_case_length <- function(query, value, fs) {
  lhs_lengths <- lengths(query)
  rhs_lengths <- lengths(value)
  all_lengths <- unique(c(lhs_lengths, rhs_lengths))

  if (length(all_lengths) == 0) {
    return(0)
  } else if (length(all_lengths) == 1) {
    return(all_lengths)
  }

  non_atomic_lengths <- all_lengths[all_lengths != 1]

  if (length(unique(non_atomic_lengths)) == 1) {
    return(non_atomic_lengths[[1]])
  }

  modal_length <- modes(non_atomic_lengths)[[1]]

  inconsistent_lhs_lengths <- setdiff(lhs_lengths, c(modal_length, 1))
  lhs_problems <- lhs_lengths %in% inconsistent_lhs_lengths

  inconsistent_rhs_lengths <- setdiff(rhs_lengths, c(modal_length, 1))
  rhs_problems <- rhs_lengths %in% inconsistent_rhs_lengths

  cli::cli_abort(c(
    "The left- and right-hand sides of all formulas must be the same length
    ({.val {modal_length}}) or length {.val {1}}.",
    if (any(lhs_problems)) {
      check_length_val(
        vapply(fs[lhs_problems], rlang::expr_text, character(1)),
        inconsistent_lhs_lengths,
        "left"
      )
    },
    if (any(rhs_problems)) {
      check_length_val(
        vapply(fs[rhs_problems], rlang::expr_text, character(1)),
        inconsistent_rhs_lengths,
        "right"
      )
    }
  ))
}

modes <- function(x) {
  counts <- table(x)
  counts <- counts[match(names(counts), unique(x))]
  modes <- names(counts[counts == max(counts)])
  mode(modes) <- mode(x)
  modes
}

check_length_val <- function(formulas, length_x, side) {
  out <- sprintf(
    "The %s-hand side of {.code %s} is length {.val {%d}}.",
    side,
    formulas,
    length_x
  )
  names(out) <- rep("x", length(out))
  out
}

