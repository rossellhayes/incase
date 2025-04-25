assert_length <- function(fs, call = rlang::caller_env()) {
  if (!length(fs)) cli::cli_abort("No cases provided", call = call)
}

compact_list <- function(...) {
  Filter(function(x) !is.null(x), rlang::list2(...))
}

coalesce_deprecated <- function(.argument, argument) {
  .arg_name <- rlang::expr_text(rlang::enexpr(.argument))
  arg_name <- rlang::expr_text(rlang::enexpr(argument))

  if (!lifecycle::is_present(argument)) {
    return(.argument)
  }

  call <- rlang::caller_call()
  fn_name <- rlang::call_name(call)

  if (.arg_name %in% names(call) & !identical(.argument, argument)) {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} and {.arg {.arg_name}} arguments cannot both be specified.",
        "*" = "Please only specify {.arg {.arg_name}}."
      ),
      call = rlang::caller_env()
    )
  }

  lifecycle::deprecate_soft(
    "0.3.3",
    glue::glue("{fn_name}({arg_name})"),
    glue::glue("{fn_name}({.arg_name})"),
    env = rlang::caller_env(),
    user_env = rlang::caller_env(2)
  )

  argument
}

# @staticimports pkg:stringstatic
#  str_remove str_subset

allow_dot_aliases <- function(
    dots,
    call = rlang::caller_call(),
    env = rlang::caller_env()
) {
  call_formals <- formals(rlang::call_name(call))

  dot_args <- str_subset(names(dots), "^\\.")

  if (length(dot_args) == 0) {
    return(dots)
  }

  undot_args <- str_remove(dot_args, "^\\.")

  for (i in sort(match(names(call_formals), undot_args), decreasing = TRUE)) {
    if (exists(undot_args[[i]], envir = env, mode = "language")) {
      dots <- append(dots, get(undot_args[[i]], envir = env), after = 0)
    }
    assign(undot_args[[i]], dots[[dot_args[[i]]]], envir = env)
    dots[dot_args[[i]]] <- NULL
  }

  dots
}
