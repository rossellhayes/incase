style <- function(x, quote, color) {
  if (!is.null(quote)) {x <- encodeString(x, quote = quote)}
  if (rlang::is_installed("crayon")) {
    x <- do.call(color, list(x), envir = asNamespace("crayon"))
  }
  x
}

code  <- function(x) {style(x, "`", "silver")}
value <- function(x) {style(x, NULL, "blue")}

abort_msg <- function(...) {
  rlang::abort(message = c(...))
}

assert_length <- function(fs) {
  if (!length(fs)) rlang::abort("No cases provided")
}
