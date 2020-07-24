#' @importFrom rlang %||%

validate_formula <- function(x, i, default_env, dots_env) {
  if (rlang::is_quosure(x)) {
    default_env <- rlang::quo_get_env(x)
    x <- rlang::quo_get_expr(x)
  }

  env <- rlang::f_env(x) %||% default_env

  list(
    lhs = rlang::new_quosure(rlang::f_lhs(x), env),
    rhs = rlang::new_quosure(rlang::f_rhs(x), env)
  )
}
