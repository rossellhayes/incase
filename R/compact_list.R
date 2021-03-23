compact_list <- function(...) {
  Filter(function(x) !is.null(x), rlang::list2(...))
}
