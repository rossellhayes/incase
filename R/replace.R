#' @importFrom rlang %||%

replace <- function(
  fs, x, default, preserve,
  fn = NULL, args = NULL,
  factor = FALSE, ordered = FALSE,
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
  m           <- validate_case_length(pairs$query, pairs$value, fs)
  out         <- rep_len(default, m)
  class(out)  <- class
  replaced    <- rep(FALSE, m)

  for (i in seq_along(pairs$query)) {
    out <- replace_with(out, pairs$query[[i]] & !replaced, pairs$value[[i]])
    replaced <- replaced | (pairs$query[[i]] & !is.na(pairs$query[[i]]))
  }

  if (factor) {
    return(factor(out, levels = unique(c(levels, out)), ordered = ordered))
  }

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
    quos_pairs, function(x) {rlang::eval_tidy(x$rhs, env = default_env)}
  )

  query <- lapply(
    quos_pairs, function(x) {rlang::eval_tidy(x$lhs, env = default_env)}
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

validate_formula <- function(x, i, default_env, dots_env) {
  if (rlang::is_quosure(x)) {
    default_env <- rlang::quo_get_env(x)
    x           <- rlang::quo_get_expr(x)
  }

  env <- rlang::f_env(x) %||% default_env

  list(
    lhs = rlang::new_quosure(rlang::f_lhs(x), env),
    rhs = rlang::new_quosure(rlang::f_rhs(x), env)
  )
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

warn_if_default <- function(default) {
  if (!is.null(default) && !is.na(default)) {
    rlang::warn(
      paste(
        code("default"), "will have no effect if", code("preserve"),
        "is", code("TRUE")
      )
    )
  }
}

replace_with <- function(x, i, val, name = NULL) {
  i[is.na(i)] <- FALSE

  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }

  x
}
