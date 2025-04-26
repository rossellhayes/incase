assert_length <- function(fs, call = rlang::caller_env()) {
  if (!length(fs)) cli::cli_abort("No cases provided", call = call)
}

compact_list <- function(...) {
  Filter(function(x) !is.null(x), rlang::list2(...))
}

first_incase_frame <- function() {
  frames <- sys.frames()

  for (i in seq_len(sys.nframe() - 1)) {
    if (
      identical(
        environment(rlang::frame_fn(frames[[i]])),
        asNamespace("incase")
      )
    ) {
      return(frames[[i]])
    }
  }

  rlang::caller_env()
}

first_incase_frame_parent <- function() {
  frames <- sys.frames()

  for (i in seq_len(sys.nframe() - 1)) {
    if (
      identical(
        environment(rlang::frame_fn(frames[[i]])),
        asNamespace("incase")
      )
    ) {
      parent <- sys.parents()[[i]]
      if (parent == 0) return(rlang::global_env())
      return(frames[[parent]])
    }
  }

  rlang::global_env()
}

coalesce_deprecated <- function(
  .argument,
  argument,
  .current_env = first_incase_frame(),
  .caller_env = first_incase_frame_parent()
) {
  .arg_name <- rlang::expr_text(rlang::enexpr(.argument))
  arg_name <- rlang::expr_text(rlang::enexpr(argument))

  if (!lifecycle::is_present(argument)) {
    return(.argument)
  }

  call <- rlang::frame_call(.current_env)
  fn_name <- rlang::call_name(call)

  if (.arg_name %in% names(call) & !identical(.argument, argument)) {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} and {.arg {(.arg_name)}} arguments cannot both be specified.",
        "*" = "Please only specify {.arg {(.arg_name)}}."
      ),
      call = .current_env
    )
  }

  lifecycle::deprecate_soft(
    "0.3.3",
    glue::glue("{fn_name}({arg_name})"),
    glue::glue("{fn_name}({.arg_name})"),
    env = .current_env,
    user_env = .caller_env
  )

  argument
}
