assert_length <- function(fs) {
  if (!length(fs)) cli::cli_abort("No cases provided")
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
