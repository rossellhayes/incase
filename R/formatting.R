if_cli <- function(symbol, fallback) {
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::symbol[[symbol]]
  } else {
    fallback
  }
}

if_crayon <- function(x, color) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    get(color, envir = asNamespace("crayon"))(x)
  } else {
    x
  }
}

code <- function(x) {
  x <- encodeString(x, quote = "`")
  x <- if_crayon(x, "silver")
  x
}

bullet <- function() {
  x <- if_cli("bullet", "*")
  x <- if_crayon(x, "blue")
  paste0("\n", x)
}

cross_bullet <- function() {
  x <- if_cli("cross", "x")
  x <- if_crayon(x, "red")
  paste0("\n", x)
}
