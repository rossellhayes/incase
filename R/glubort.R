glubort <- function(..., .zero = FALSE, .abort = rlang::abort) {
  glue <- if (zero) {paste0} else {paste}
  text <- glue(..., .sep = " ")
  .abort(text)
}
