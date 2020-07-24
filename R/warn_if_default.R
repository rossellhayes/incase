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
