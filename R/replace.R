replace <- function(
  fs, x, default, preserve, fn = NULL, args = NULL, factor = FALSE,
  default_env, current_env
) {
  assert_length(fs)

  pairs <- extract_formula_pairs(fs, x, fn, args, default_env, current_env)

  if (factor) {levels <- as.character(c(pairs$value, recursive = TRUE))}

  if (preserve) {
    warn_if_default(default)
    pairs$query[[length(pairs$query) + 1]] <- TRUE
    pairs$value[[length(pairs$value) + 1]] <- x
  }

  class       <- class(c(pairs$value, recursive = TRUE))
  pairs$value <- lapply(pairs$value, `class<-`, class)
  m           <- validate_case_when_length(pairs$query, pairs$value, fs)
  out         <- rep_len(default, m)
  class(out)  <- class
  replaced    <- rep(FALSE, m)

  for (i in seq_along(pairs$query)) {
    out <- replace_with(out, pairs$query[[i]] & !replaced, pairs$value[[i]])
    replaced <- replaced | (pairs$query[[i]] & !is.na(pairs$query[[i]]))
  }

  if (factor) {out <- factor(out, levels = unique(c(levels, out)))}

  out
}

extract_formula_pairs <- function(
  fs, x = NULL, fn = NULL, args = NULL, default_env, current_env,
  assert_logical_lhs = TRUE
) {
  quos_pairs <- Map(
    function(x, i) {
      validate_formula(x, i, default_env = default_env, dots_env = current_env)
    },
    fs, seq_along(fs)
  )

  value <- lapply(
    quos_pairs, function(x) {rlang::eval_tidy(x[["rhs"]], env = default_env)}
  )

  query <- lapply(
    quos_pairs, function(x) {rlang::eval_tidy(x[["lhs"]], env = default_env)}
  )

  if (!is.null(fn)) {
    query <- lapply(
      query, function(query) {
        do.call(rlang::as_function(fn), c(list(x, query), args))
      }
    )
  }

  if (assert_logical_lhs) {assert_logical_lhs(query, quos_pairs)}

  list(value = value, query = query)
}


replace_with <- function(x, i, val, name = NULL) {
  check_length_val(length(val), length(x), name)

  i[is.na(i)] <- FALSE

  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }

  x
}
